#include "../context.h"
#include "compiler/program_builder.h"

namespace adder {
  namespace compiler {
    bool prepare_call(ast const & ast, program_builder * program, std::optional<size_t> const & parameters);
    bool generate_call(ast const & ast, program_builder * program);

    bool generate_copy(program_builder * program, size_t statementId) {
      unused(program, statementId);
      return false;
    }

    bool generate_code(ast const & ast, program_builder * program, size_t statementId);

    bool generate_literal_code(program_builder * program, bool value) {
      program_builder::value result;
      result.constant.emplace(0);
      std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_info = program->meta->get_type_reference(type_primitive::bool_);
      program->push_value(result);
      return true;
    }

    bool generate_literal_code(program_builder * program, int64_t value) {
      program_builder::value result;
      result.constant.emplace(0);
      std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_info = program->meta->get_type_reference(type_primitive::int64);
      program->push_value(result);
      return true;
    }

    bool generate_literal_code(program_builder * program, double value) {
      program_builder::value result;
      result.constant.emplace(0);
      std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_info = program->meta->get_type_reference(type_primitive::float64);
      program->push_value(result);
      return true;
    }

    bool generate_literal_code(program_builder * program, std::string_view const & value) {
      unused(value);

      program_builder::value result;
      // result.constant.emplace(0);
      // result.symbol = program->push_data_symbol(value.data(), value.length());
      // std::memcpy(&result.constant.value(), &value, sizeof(value));
      // result.type_info  = program->meta->get_type_reference(type_primitive::float64);
      result.type_info = program->meta->get_type_reference("char[]");
      program->push_value(result);
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::literal const & statement, size_t statementId) {
      unused(ast, statementId);

      return std::visit([&](auto && value) { return generate_literal_code(program, value); }, statement.value);
    }

    bool generate_code(ast const & ast, program_builder * program, expr::identifier const & statement, size_t statementId) {
      unused(ast);

      std::optional<program_builder::value> variable = program->find_value_by_identifier(statement.name);
      if (variable.has_value()) {
        program->push_value(variable.value());
        return true;
      }

      // Allow referencing functions defined anywhere in an outer scope
      const auto symbolIndex = program->meta->statement_info[statementId].symbol_index;
      if (symbolIndex.has_value()) {
        const auto & symbol = program->meta->symbol_references[symbolIndex.value()];
        if (!symbol.get().has_local_storage() && symbol.get().is_function()) {
          program_builder::value fn;
          fn.symbol_index = symbol;
          fn.type_info    = symbol.get_type();
          program->push_value(fn);
          return true;
        }
      }

      printf("Error: Undeclared identifier '%.*s'\n", (int)statement.name.length(), statement.name.data());
      // Push Error: Undeclared identifier `statement.name`
      return false;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::list const & statement, size_t statementId) {
      unused(statementId);
      if (!generate_code(ast, program, statement.expr))
        return false;
      if (statement.next.has_value())
        generate_code(ast, program, statement.next.value());
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::type_name const & statement, size_t statementId) {
      unused(ast, program, statement, statementId);
      printf("info: No code generated for type_name expression\n");
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::type_fn const & statement, size_t statementId) {
      unused(ast, program, statement, statementId);
      printf("info: No code generated for type_fn expression\n");
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::type_modifier const & statement,
                       size_t statementId) {
      unused(ast, program, statement, statementId);
      printf("info: No code generated for type_modifier expression\n");
      return true;
    }

    bool initialize_variable(ast const & ast, program_builder * program, program_builder::value const & receiver,
                             program_builder::value const & initializer) {
      assert(receiver.type_info.has_value());
      assert(!receiver.constant.has_value());
      assert(initializer.type_info.has_value());

      if (is_reference_of(receiver.type_info, initializer.type_info)) {
        // Explicitly init references to types.
        auto address  = program->load_address_of(initializer);
        auto variable = program->load_address_of(receiver);
        program->store(address, variable, sizeof(vm::register_value));
        program->release_register(variable);
        program->release_register(address);
        return true;
      }

      std::optional<program_builder::value> unnamedInit =
        program->find_unnamed_initializer(receiver.type_info.value(), initializer.type_info.value());

      if (!unnamedInit.has_value()) {
        auto a = receiver.type_info->get_identifier();
        auto b = initializer.type_info->get_identifier();
        printf("Error: No unnamed initializer that can create a `%.*s` from `%.*s`\n", (int)a.length(), a.data(),
               (int)b.length(), b.data());
        // TODO: Push error. No unnamed initializer that can create a `receiver` from `initializer`
        return false;
      }

      program->push_value({}); // Null return value
      program->push_value(initializer);
      program->push_value(receiver);
      program->push_value(unnamedInit.value());
      if (!generate_call(ast, program)) {
        return false;
      }

      // Pop return value of call
      program->pop_value();
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::variable_declaration const & statement,
                       size_t statementId) {
      const size_t temporaries = program->scopes.back().temporaries.size(); // TODO: Scoped helper for this?

      const type_reference variableType = program->meta->get_type_reference(statement.type.value()).value();
      const auto           symbol       = program->meta->get_statement_symbol(statementId);

      program_builder::value receiver;
      if (symbol->get().has_local_storage()) {
        receiver = program->allocate_stack_variable(variableType);
      } else {
        receiver.type_info    = variableType;
        receiver.symbol_index = symbol;
      }
      receiver.identifier = statement.name;

      program->add_variable(receiver);

      if (statement.initializer.has_value()) {
        const size_t count = program->value_stack.size();
        if (!generate_code(ast, program, statement.initializer.value())) {
          printf("Error: failed to generate code for initializer statement of symbol '%s'\n",
                 symbol->get().full_identifier.c_str());
          return false;
        }

        assert(count < program->value_stack.size());
        adder::unused(count);

        auto initializer = program->pop_value();
        assert(initializer.has_value());
        if (!initialize_variable(ast, program, receiver, initializer.value())) {
          printf("Error: failed initialize symbol '%s' from result of the initializer statement\n",
                 symbol->get().full_identifier.c_str());
          return false;
        }
      }

      while (program->scopes.back().temporaries.size() > temporaries)
        program->free_temporary_value();

      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::init const & statement, size_t statementId) {
      unused(ast, program, statement, statementId);
      return true;
      // generate_code(program, statement.expression);
      // auto value = program->pop_expression_result();
      //
      // generate_code(program, statement.target);
      // auto target = program->pop_expression_result();
      //
      // if (!target.type_index.has_value() || value.type_index.has_value())
      //   return false;
      //
      // type const & targetType = program->types[target.type_index.value()];
      // type const & valueType = program->types[value.type_index.value()];
      //
      // return std::visit([&](const auto & a) {
      //   return std::visit([&](const auto & b) {
      //     return generate_init_code(program, target, a, value, b);
      //     }, valueType.desc);
      // }, targetType.desc);
    }

    bool generate_code(ast const & ast, program_builder * program, expr::function_return const & statement,
                       size_t statementId) {
      unused(statementId);

      auto receiver = program->get_return_value();
      if (is_undefined(receiver.type_info)) {
        auto symbol = program->current_function().symbol.get();
        printf("Error: fn %.*s return type is undefined.\n", (int)symbol.name.length(), symbol.name.data());
        return false;
      }

      if (is_void(receiver.type_info)) {
        if (statement.expression.has_value()) {
          auto symbol = program->current_function().symbol.get();
          printf("Error: fn %.*s returns void, not a value\n", (int)symbol.name.length(), symbol.name.data());
          return false;
        }
        return true;
      }

      if (!generate_code(ast, program, statement.expression.value())) {
        printf("Error: failed to evaluate return expression\n");
        return false;
      }

      auto expressionResult = program->pop_value();
      if (!expressionResult.has_value()) {
        printf("Error: return expression did not evaluate to a value\n");
        return false;
      }

      if (!initialize_variable(ast, program, receiver, expressionResult.value())) {
        printf("Error: failed to initialize return value\n");
        return false;
      }

      program->return_with_return_handler();
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::binary_operator const & statement,
                       size_t statementId) {
      switch (statement.type_name) {
      case expr::operator_type::call: {
        if (!generate_code(ast, program, statement.left.value())) {
          printf("Error: failed to evaluate lhs of operator\n");
          return false;
        }

        // TODO: When generating code,
        //      [optimize] If a temporary can be forwarded as a parameter, it should be allocated in the correct place.
        if (!(prepare_call(ast, program, statement.right) && generate_call(ast, program)))
          return false;

        break;
      }
      default: {
        auto callableSymbol = program->meta->get_statement_symbol(statementId);
        if (!callableSymbol.has_value()) {
          printf("Error: no suitable binary operator\n");
          return false;
        }

        if (!(generate_code(ast, program, statement.right.value()) && generate_code(ast, program, statement.left.value())))
          return false;
        auto lhs = program->pop_value();
        auto rhs = program->pop_value();

        program_builder::value function;
        function.type_info    = callableSymbol->get_type();
        function.symbol_index = callableSymbol.value();
        function.identifier   = expr::get_operator_identifer(statement.type_name);

        auto ret = program->allocate_temporary_value(return_type_of(function.type_info).value());
        program->push_value(ret);
        program->push_value(rhs.value());
        program->push_value(lhs.value());
        program->push_value(function);

        if (!generate_call(ast, program))
          return false;

        break;
      }
      }
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::function_declaration const & statement,
                       size_t statementId) {
      const auto symbolIndex = program->meta->get_statement_symbol(statementId);
      if (!symbolIndex.has_value()) {
        // TODO: Report error. Declaration does not have a valid symbol
        printf("Error: No symbol associated with function declaration\n");
        return false;
      }

      if ((statement.flags & symbol_flags::extern_) == symbol_flags::extern_) {
        if (!program->begin_function(symbolIndex.value())) {
          return false;
        }
        program->call_native(symbolIndex.value());
        program->ret();
        program->end_function();
      } else if (statement.body.has_value() && (statement.flags & symbol_flags::inline_) == symbol_flags::none) {
        if (!program->begin_function(symbolIndex.value())) {
          return false;
        }
        program->push_return_pointer();
        program->push_frame_pointer();
        program->move(vm::register_names::fp, vm::register_names::sp);

        program->begin_scope();

        size_t rootScope = program->scopes.size();
        program->push_return_handler([rootScope](auto * program) {
          program->emit_scope_cleanup(rootScope);
          program->jump_relative(0);
          program->set_instruction_tag(program_builder::instruction_tag::return_jmp);
        });

        auto &  func = program->current_function();
        int64_t nextArgOffset =
          -(int64_t)func.args_size - program_builder::function::CallLinkStorageSize; // Frame pointer + return pointer

        for (auto argId : statement.arguments) {
          const auto & decl      = ast.get<expr::variable_declaration>(argId);
          const auto & argSymbol = program->meta->get_statement_symbol(argId);
          if (!argSymbol.has_value()) {
            printf("Error: Unknown argument type\n");
            return false;
          }

          program_builder::value val;
          val.symbol_index            = argSymbol;
          val.type_info               = argSymbol->get_type();
          val.indirect_register_index = (vm::register_index)vm::register_names::fp;
          val.address_offset          = nextArgOffset;
          val.identifier              = decl.name;
          nextArgOffset += val.type_info->get_size();

          program->add_variable(val);
        }

        if (!generate_code(ast, program, statement.body.value())) {
          printf("Error: Failed to generate code for function\n");
          return false;
        }

        program->end_scope();
        program->pop_frame_pointer();
        program->pop_return_pointer();
        program->ret();
        program->end_function();

        program->pop_return_handler();
      }

      program_builder::value val;
      val.symbol_index = symbolIndex;
      val.type_info    = symbolIndex->get_type();
      val.identifier   = statement.identifier;
      program->add_variable(val);

      unused(ast, program, statement);
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::call_parameter const & statement,
                       size_t statementId) {
      unused(statementId);

      generate_code(ast, program, statement.expression);

      if (statement.next.has_value())
        generate_code(ast, program, statement.next.value());

      return true;
    }

    bool push_argument(ast const & ast, program_builder * program, std::string_view const & name, program_builder::value src,
                       type_reference argType, bool isInline) {
      unused(ast, program, src, argType);

      if (isInline) {
        if (src.type_info == argType) {
          auto cpy       = src;
          cpy.identifier = name;
          cpy.flags |= program_builder::value_flags::alias;
          program->add_variable(cpy);
          return true;
        } else if (argType.is_reference_of(src.type_info)) {
          auto receiver       = src;
          receiver.identifier = name;
          // Method expects a reference, so treat the value alias with reference semantics
          receiver.flags |= program_builder::value_flags::eval_as_reference;
          receiver.flags |= program_builder::value_flags::alias;
          program->add_variable(receiver);
          return true;
        } else {
          auto receiver       = program->allocate_temporary_value(argType);
          receiver.identifier = name;
          program->add_variable(receiver);
          return initialize_variable(ast, program, receiver, src);
        }
      }

      auto receiverId     = program->allocate_temporary_call_parameter(argType);
      auto receiver       = program->get_temporary(receiverId);
      receiver.identifier = name;
      program->add_variable(receiver);

      if (src.type_info == argType) {
        auto unnamedInit = program->find_unnamed_initializer(argType, argType);

        if (!unnamedInit.has_value()) {
          auto a = argType.get_identifier();
          auto b = argType.get_identifier();
          printf("Error: No unnamed initializer that can create a `%.*s` from `%.*s`\n", (int)a.length(), a.data(),
                 (int)b.length(), b.data());
          return false;
        }

        program->push_value({}); // Void return value
        program->push_value(src);
        program->push_value(receiver);
        program->push_value(unnamedInit.value());
        if (!generate_call(ast, program)) {
          return false;
        }
        program->pop_value();
        return true;
      } else {
        return initialize_variable(ast, program, receiver, src);
      }
    }

    bool prepare_call_parameters_reversed(ast const & tree, program_builder * program, std::optional<size_t> const & id) {
      if (!id.has_value())
        return true;

      auto param = tree.get<expr::call_parameter>(id.value());
      if (!prepare_call_parameters_reversed(tree, program, param.next)) {
        return false;
      }

      size_t prevSz = program->value_stack.size();
      unused(prevSz);

      if (!generate_code(tree, program, param.expression)) {
        return false;
      }

      assert(program->value_stack.size() != prevSz);

      return true;
    }

    bool prepare_call(ast const & tree, program_builder * program, std::optional<size_t> const & parameters) {
      auto function = program->pop_value();
      if (!function.has_value()) {
        printf("Error: lhs of operator did not evaluate to a value\n");
        return false;
      }

      if (is_undefined(function->type_info)) {
        // TODO: Push error. No type
        printf("Error: Callable type is undefined\n");
        return false;
      }

      if (!is_function(function->type_info)) {
        // TODO: Push error. Not callable
        auto typeName = function->type_info->get_identifier();
        printf("Error: %.*s is not callable\n", (int)typeName.length(), typeName.data());
        return false;
      }

      auto ret = program->allocate_temporary_value(function->type_info->return_type().value());
      program->push_value(ret);

      if (!prepare_call_parameters_reversed(tree, program, parameters)) {
        printf("Error: Failed to prepare call parameters\n");
        return false;
      }

      program->push_value(function.value());
      return true;
    }

    /// Generate a call using expressions pushed to the builders result stack.
    bool generate_call(ast const & ast, program_builder * program) {
      std::optional<program_builder::value> function = program->pop_value();

      if (!function.has_value() || !is_function(function->type_info)) {
        // Push error: First expression must be a callable symbol.
        return false;
      }

      std::optional<symbol> callable;
      if (function->symbol_index.has_value())
        callable = function->symbol_index->get();

      auto decayedSymbolType = remove_reference(decay_type(function->type_info));

      // Pointer to the actual function definition.
      // Allows us to inline the call if possible.
      // assert(false &&
      //   "To inline across module imports we need the ast of the imported module"
      //   "Consider moving AST into program_metadata and having a 'symbol_reference' type similar to 'type_reference'"
      //   "Alternative to 'symbol_reference' would be 'expr_reference', but that may be excessive."
      // );
      expr::function_declaration const * func =
        callable.has_value() && callable->declaration_id.has_value() &&
            function->symbol_index->meta->tree.is<expr::function_declaration>(callable->declaration_id.value())
          ? &function->symbol_index->meta->tree.get<expr::function_declaration>(callable->declaration_id.value())
          : nullptr;

      type_function const * signature = decayedSymbolType->try_get_as<type_function>();

      if (signature == nullptr) {
        // Push error: Type is not callable.
        printf("Error: %.*s is not a callable type\n", (int)function->type_info->get_identifier().length(),
               function->type_info->get_identifier().data());
        return false;
      }

      const size_t prevTemporaryCount = program->scopes.back().temporaries.size();
      const bool   inlineCall         = func != nullptr && (func->flags & symbol_flags::inline_) == symbol_flags::inline_;

      if (inlineCall) {
        // TODO: Fix me - inlining functions that allocate stack space is probably broken.
        program->begin_scope();
        const size_t rootScope   = program->scopes.size();
        const size_t temporaries = program->scopes.back().temporaries.size();

        program->push_return_handler([rootScope](auto * program) {
          program->emit_scope_cleanup(rootScope);
          program->jump_relative(0);
          program->set_instruction_tag(program_builder::instruction_tag::return_jmp);
        });

        for (size_t i = 0; i < signature->arguments.size(); ++i) {
          auto & var = function->symbol_index->meta->tree.get<expr::variable_declaration>(func->arguments[i]);
          auto   arg = program->pop_value();
          if (!arg.has_value()) {
            return false;
          }

          push_argument(ast, program, var.name, arg.value(), signature->arguments[i], inlineCall);
        }

        program->push_return_value_receiver(program->value_stack.back());

        const size_t startInstruction = program->current_function().instructions.size();

        if (func->body.has_value()) {
          // Use functions ast (might be from another module)
          if (!generate_code(function->symbol_index->meta->tree, program, func->body.value())) {
            return false;
          }
        }

        program->pop_return_value_receiver();

        {
          auto & curFunc = program->current_function();
          for (size_t i = startInstruction; i < curFunc.instructions.size(); ++i) {
            auto & op  = curFunc.instructions[i];
            auto & tag = curFunc.instruction_tags[i];
            switch (tag) {
            case program_builder::instruction_tag::return_jmp: {
              // Jump to return statement
              assert(op.code == vm::op_code::jump_relative && "invalid op code tagged with instruction_tag::return_jmp");
              op.jump_relative.offset     = (curFunc.instructions.size() - i) * sizeof(vm::instruction);
              curFunc.instruction_tags[i] = program_builder::instruction_tag::none; // Clear tag.
              break;
            }
            }
          }
        }

        while (program->scopes.back().temporaries.size() > temporaries)
          program->free_temporary_value();

        program->end_scope();
      } else {
        program->begin_scope();

        auto   returnType = function->type_info->return_type().value();
        size_t rv         = program->allocate_temporary_call_parameter(returnType);

        for (size_t i = 0; i < signature->arguments.size(); ++i) {
          auto arg = program->pop_value();
          if (!arg.has_value()) {
            return false;
          }
          push_argument(ast, program, "", arg.value(), signature->arguments[i], inlineCall);
        }

        program->call(function.value());

        auto returnedValue = program->value_stack.back();
        if (!(returnType.is_void() && returnedValue.type_info->is_void()))
          initialize_variable(ast, program, returnedValue, program->get_temporary(rv));

        // Free args space
        for (size_t i = 0; i < signature->arguments.size(); ++i) {
          program->free_temporary_value();
        }

        // Free return space
        program->free_temporary_value();
        program->end_scope();
      }

      while (program->scopes.back().temporaries.size() > prevTemporaryCount)
        program->free_temporary_value();

      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::class_decl const & statement, size_t statementId) {
      unused(ast, program, statement, statementId);
      return true;
    }

    void destroy_symbol(ast const & ast, program_builder * program, size_t symbolId) {
      unused(ast, program, symbolId);
    }

    bool generate_code(ast const & ast, program_builder * program, expr::block const & scope, size_t blockId) {
      unused(blockId);

      auto scopeId = program->meta->statement_info[blockId].scope_index;
      if (!scopeId.has_value()) {
        // No associated scope
        return false;
      }

      program->begin_scope();

      const auto & scopeMeta    = program->meta->scopes[scopeId.value()];
      const bool   isStackFrame = !scopeMeta.parent_function_scope.has_value();
      const bool   isInlining   = program->current_function().scope_id != scopeId;

      if (isStackFrame && !isInlining) {
        // Is function stack frame
        program->alloc_stack(1);
        program->set_instruction_tag(program_builder::instruction_tag::stack_frame);
      }

      for (size_t statementId : scope.statements) {
        size_t temporaries = program->scopes.back().temporaries.size();

        if (!generate_code(ast, program, statementId)) {
          printf("Failed to generate code for statement: %lld\n", statementId);
          return false;
        }

        // Destroy any dangling temporaries
        while (program->scopes.back().temporaries.size() > temporaries)
          program->free_temporary_value();
      }

      // If the last statement was a return, scope variables will have already been cleaned up.
      // skip cleanup instructions.
      if (scope.statements.empty() || !ast.is<expr::function_return>(scope.statements.back()))
        program->emit_scope_cleanup();

      if (isStackFrame && !isInlining) {
        // Mark return section
        auto & func = program->current_function();

        func.return_section_start = func.instructions.size();

        program->free_stack(1);
        program->set_instruction_tag(program_builder::instruction_tag::stack_frame);
      }

      program->end_scope();
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::branch const & branch, size_t statementId) {
      unused(statementId);

      if (!generate_code(ast, program, branch.condition)) {
        printf("Error: failed to generate code for if condition\n");
        return false;
      }

      auto condition = program->pop_value();
      if (!condition.has_value()) {
        printf("Error: if condition did not resolve to a value\n");
        return false;
      }
      auto booleanCond = condition.value();
      if (!is_bool(condition->type_info)) {
        booleanCond = program->allocate_temporary_value(program->meta->get_type_reference(type_primitive::bool_).value());
        initialize_variable(ast, program, booleanCond, condition.value());
      }

      vm::register_index val                       = program->load_value_of(booleanCond);
      const size_t       skipTrueBranchInstruction = program->current_function().instructions.size();
      program->jump_if_zero_rel(0, val);
      program->release_register(val);

      if (!generate_code(ast, program, branch.true_branch)) {
        printf("Error: failed to generate code for if body\n");
        return false;
      }
      size_t trueBranchSize = 0;
      if (branch.false_branch.has_value()) {
        size_t skipFalseBranchInstruction = program->current_function().instructions.size();
        program->jump_relative(0);
        trueBranchSize = program->current_function().instructions.size() - skipTrueBranchInstruction;

        if (!generate_code(ast, program, branch.false_branch.value())) {
          printf("Error: failed to generate code for else/elseif body\n");
          return false;
        }

        const size_t falseBranchSize = program->current_function().instructions.size() - skipFalseBranchInstruction;
        program->current_function().instructions[skipFalseBranchInstruction].jump_relative.offset =
          falseBranchSize * sizeof(vm::instruction);
      } else {
        trueBranchSize = program->current_function().instructions.size() - skipTrueBranchInstruction;
      }
      program->current_function().instructions[skipTrueBranchInstruction].jump_relative.offset =
        trueBranchSize * sizeof(vm::instruction);
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::loop const & loop, size_t statementId) {
      unused(statementId);

      program->begin_scope();

      if (loop.pre.has_value()) {
        // Generate pre-condition code
        program_builder::scoped_temporary_cleaner tempCleanup(program, program->scopes.size() - 1);
        if (!generate_code(ast, program, loop.pre.value())) {
          printf("Error: loop pre-statement is not valid\n");
          return false;
        }
      }

      std::optional<size_t> skipBranchInstruction;
      const size_t          topOfLoop = program->current_function().instructions.size();

      { // Check branch condition
        program_builder::scoped_temporary_cleaner tempCleanup(program, program->scopes.size() - 1);

        std::optional<program_builder::value> condition;
        if (loop.condition.has_value()) {
          if (!generate_code(ast, program, loop.condition.value())) {
            printf("Error: failed to generate code for if condition\n");
            return false;
          }

          auto result = program->pop_value();
          if (!result.has_value()) {
            printf("Error: if condition did not resolve to a value\n");
            return false;
          }

          condition = result;
          if (!is_bool(condition->type_info)) {
            condition = program->allocate_temporary_value(program->meta->get_type_reference(type_primitive::bool_).value());
            initialize_variable(ast, program, condition.value(), result.value());
          }
        } else {
          condition.emplace();
          condition->constant  = 1;
          condition->type_info = program->meta->get_type_reference(type_primitive::bool_);
        }

        vm::register_index val = program->load_value_of(condition.value());
        skipBranchInstruction  = program->current_function().instructions.size();
        program->jump_if_zero_rel(0, val);
        program->release_register(val);
      }

      if (loop.body.has_value()) {
        program_builder::scoped_temporary_cleaner tempCleanup(program, program->scopes.size() - 1);

        if (!generate_code(ast, program, loop.body.value())) {
          printf("Error: failed to generate code for loop body\n");
          return false;
        }
      }

      if (loop.post.has_value()) {
        program_builder::scoped_temporary_cleaner tempCleanup(program, program->scopes.size() - 1);
        if (!generate_code(ast, program, loop.post.value())) {
          printf("Error: loop post-statement is not valid\n");
          return false;
        }
      }

      // Jump to top of loop
      const int64_t offsetToTop = (topOfLoop - program->current_function().instructions.size()) * sizeof(vm::instruction);
      program->jump_relative(offsetToTop);

      if (skipBranchInstruction.has_value()) {
        const int64_t bodySize = program->current_function().instructions.size() - skipBranchInstruction.value();
        program->current_function().instructions[skipBranchInstruction.value()].jump_relative.offset =
          bodySize * sizeof(vm::instruction);
      }

      program->end_scope();

      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::byte_code const & code, size_t statementId) {
      unused(ast, statementId);

      return code.callback != nullptr && code.callback(program);
    }

    bool generate_code(ast const & ast, program_builder * program, expr::import_symbol const & code, size_t statementId) {
      unused(ast, code);
      for (size_t i = 0; i < program->meta->get_statement_symbol_count(statementId); ++i) {
        auto   symbol      = program->meta->get_statement_symbol(statementId, i);

        program_builder::value receiver;
        receiver.type_info    = symbol->get_type();
        receiver.symbol_index = symbol;
        receiver.identifier   = code.symbol_alias;
        program->add_variable(receiver);
      }      
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::import_module const & code, size_t statementId) {
      unused(ast, code);
      for (size_t i = 0; i < program->meta->get_statement_symbol_count(statementId); ++i) {
        auto   symbol      = program->meta->get_statement_symbol(statementId, i);
        program_builder::value receiver;
        receiver.type_info    = symbol->get_type();
        receiver.identifier   = symbol->get().name;
        receiver.symbol_index = symbol;
        program->add_variable(receiver);
      }
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::type_declaration const & decl, size_t statementId) {
      unused(ast, program, decl, statementId);
      // No-op
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, size_t statementId) {
      bool result = false;
      std::visit([&](auto const & statement) {
        result = generate_code(ast, program, statement, statementId);
      }, ast.statements[statementId]);
      return result;
    }

    std::optional<program> phase3_generate_code(std::shared_ptr<program_metadata> const & meta) {
      program_builder ret;
      ret.meta = meta;

      auto moduleInit = meta->find_symbol("()=>void:$module_init");
      ret.begin_function(moduleInit.value());
      ret.begin_scope();

      // Push frame for initializer func
      ret.push_return_pointer();
      ret.push_frame_pointer();
      ret.move(vm::register_names::fp, vm::register_names::sp);

      expr::block const & top = meta->tree.get<expr::block>(meta->tree.statements.size() - 1);
      for (size_t statementId : top.statements) {
        if (!generate_code(meta->tree, &ret, statementId)) {
          return std::nullopt;
        }
      }

      // Pop frame for initializer func
      ret.pop_frame_pointer();
      ret.pop_return_pointer();
      ret.ret();
      ret.end_scope();
      ret.end_function();

      return ret.binary();
    }
  }
}