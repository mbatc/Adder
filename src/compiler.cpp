#include "compiler.h"
#include "common.h"
#include "vm.h"
#include "program.h"

#include "compiler/lexer.h"
#include "compiler/ast.h"
#include "compiler/parser.h"
#include "compiler/ast/expressions.h"
#include "compiler/program_builder.h"

#include <memory>
#include <map>
#include <variant>
#include <optional>
#include <functional>
#include <algorithm>

namespace adder {
  namespace compiler {
    size_t eval_decltype(ast const& ast, program_metadata * meta, size_t statementId);

    template<typename T>
    size_t eval_decltype_impl(ast const& ast, program_metadata * meta, size_t statementId, T const & o) {
      unused(ast, o);

      auto metaTypeId = meta->statement_info[statementId].type_id;
      return metaTypeId.value_or(meta->get_type_index(get_primitive_type_name(type_primitive::void_)).value());
    }

    size_t eval_decltype_impl(ast const & ast, program_metadata * meta, size_t statementId, expr::identifier const & identifier) {
      unused(ast, identifier);

      auto symbolIndex = meta->statement_info[statementId].symbol_index;
      if (!symbolIndex.has_value()) {
        return meta->get_type_index(get_primitive_type_name(type_primitive::void_)).value();
      }
      return meta->symbols[symbolIndex.value()].type;
    }

    size_t eval_decltype_impl(ast const & ast, program_metadata * meta, size_t statementId, expr::binary_operator const & op) {
      unused(statementId);
      // meta->is_valid_function_overload();
      if (op.type_name == expr::operator_type::call) {
        size_t funcType = eval_decltype(ast, meta, op.left.value());
        assert(meta->is_function(funcType));
        auto returnType = meta->return_type_of(funcType);
        return returnType.value_or(meta->get_type_index(get_primitive_type_name(type_primitive::void_)).value());
      }
      else {
        // TODO: Get operator method and eval return type
      }
      return false;
    }

    size_t eval_decltype(ast const & ast, program_metadata * meta, size_t statementId) {
      return std::visit([&](auto&& o) { return eval_decltype_impl(ast, meta, statementId, o); }, ast.statements[statementId]);
    }

    bool prepare_operator_call(program_builder* program, expr::operator_type);
    bool prepare_call(ast const& ast, program_builder* program, std::optional<size_t> const& parameters);
    bool generate_call(ast const & ast, program_builder * program);

    bool generate_copy(ast const & ast, program_builder * program, size_t statementId) {
      unused(ast, program, statementId);
      return false;
    }

    bool generate_code(ast const& ast, program_builder* program, size_t statementId);

    bool generate_literal_code(ast const & ast, program_builder* program, bool value) {
      unused(ast);

      program_builder::value result;
      result.constant.emplace(0);
      std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_index = program->meta.get_type_index("bool");
      program->push_value(result);
      return true;
    }

    bool generate_literal_code(ast const & ast, program_builder* program, int64_t value) {
      unused(ast);

      program_builder::value result;
      result.constant.emplace(0);
      std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_index = program->meta.get_type_index("int64");
      program->push_value(result);
      return true;
    }

    bool generate_literal_code(ast const & ast, program_builder* program, double value) {
      unused(ast);

      program_builder::value result;
      result.constant.emplace(0);
      std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_index = program->meta.get_type_index("float64");
      program->push_value(result);
      return true;
    }

    bool generate_literal_code(ast const & ast, program_builder * program, std::string_view const & value) {
      unused(ast, value);

      program_builder::value result;
      // result.constant.emplace(0);
      // result.symbol = program->push_data_symbol(value.data(), value.length());
      // std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_index = program->meta.get_type_index("char[]");
      program->push_value(result);
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::literal const & statement, size_t statementId) {
      unused(statementId);

      return std::visit(
        [&](auto &&value) {
          return generate_literal_code(ast, program, value);
        },
        statement.value);
    }

    bool generate_code(ast const & ast, program_builder * program, expr::identifier const & statement, size_t statementId) {
      unused(ast, statement, statementId);
      std::optional<program_builder::value> value = program->find_value_by_identifier(statement.name);

      if (!value.has_value()) {
        printf("Error: Undeclared identifier '%.*s'\n", (int)statement.name.length(), statement.name.data());
        // Push Error: Undeclared identifier `statement.name`
        return false;
      }

      program->push_value(value.value());
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::list const & statement, size_t statementId) {
      unused(ast, program, statement, statementId);
      printf("info: No code generated for list expression\n");
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

    bool generate_code(ast const & ast, program_builder * program, expr::type_modifier const & statement, size_t statementId) {
      unused(ast, program, statement, statementId);
      printf("info: No code generated for type_modifier expression\n");
      return true;
    }

    bool initialize_variable(ast const & ast, program_builder * program, program_builder::value const & receiver, program_builder::value const & initializer) {
      assert(receiver.type_index.has_value());
      assert(!receiver.constant.has_value());
      assert(initializer.type_index.has_value());

      if (program->meta.is_reference_of(receiver.type_index.value(), initializer.type_index.value())) {
        // Explicitly init references to types.
        auto address  = program->load_address_of(initializer);
        auto variable = program->load_address_of(receiver);
        program->store(address, variable, sizeof(vm::register_value));
        program->release_register(variable);
        program->release_register(address);
        return true;
      }
      
      std::optional<program_builder::value> unnamedInit = program->find_unnamed_initializer(receiver.type_index.value(), initializer.type_index.value());

      if (!unnamedInit.has_value()) {
        auto a = program->meta.types[receiver.type_index.value()];
        auto b = program->meta.types[initializer.type_index.value()];
        printf("Error: No unnamed initializer that can create a `%.*s` from `%.*s`\n",
          (int)a.identifier.length(), a.identifier.data(),
          (int)b.identifier.length(), b.identifier.data());
        // TODO: Push error. No unnamed initializer that can create a `receiver` from `initializer`
        return false;
      }

      // program->allocate_temporary_value(returnType.value());
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

    bool generate_code(ast const & ast, program_builder * program, expr::variable_declaration const & statement, size_t statementId) {
      const size_t temporaries = program->scopes.back().temporaries.size(); // TODO: Scoped helper for this?

      const size_t variableType = program->meta.get_type_index(ast, statement.type.value()).value();
      const size_t symbolIndex  = program->meta.statement_info[statementId].symbol_index.value();
      const auto & symbol       = program->meta.symbols[symbolIndex];

      program_builder::value receiver;
      if (symbol.has_local_storage()) {
        receiver = program->allocate_stack_variable(variableType);
      }
      else {
        receiver.type_index = variableType;
        receiver.symbol_index = symbolIndex;
      }
      receiver.identifier = statement.name;

      program->add_variable(receiver);

      if (statement.initializer.has_value()) {
        const size_t count = program->value_stack.size();
        if (!generate_code(ast, program, statement.initializer.value())) {
          printf("Error: failed to generate code for initializer statement of symbol '%s'\n", symbol.full_identifier.c_str());
          return false;
        }

        assert(count < program->value_stack.size()); adder::unused(count);

        auto initializer = program->pop_value();
        assert(initializer.has_value());
        if (!initialize_variable(ast, program, receiver, initializer.value())) {
          printf("Error: failed initialize symbol '%s' from result of the initializer statement\n", symbol.full_identifier.c_str());
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
      // generate_code(ast, program, statement.expression);
      // auto value = program->pop_expression_result();
      // 
      // generate_code(ast, program, statement.target);
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
      //     return generate_init_code(ast, program, target, a, value, b);
      //     }, valueType.desc);
      // }, targetType.desc);
    }

    bool generate_code(ast const & ast, program_builder * program, expr::function_return const & statement, size_t statementId) {
      unused(statementId);

      auto receiver = program->get_return_value();
      if (!receiver.type_index.has_value()) {
        return false;
      }

      if (program->meta.is_void(receiver.type_index)) {
        if (statement.expression.has_value()) {
          auto symbol = program->meta.symbols[program->current_function().symbol];

          // TODO: Push error. Unexpected expression for function that returns void.
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

    bool generate_code(ast const & ast, program_builder * program, expr::binary_operator const & statement, size_t statementId) {
      switch (statement.type_name) {
      case expr::operator_type::call: {
        if (!generate_code(ast, program, statement.left.value())) {
          printf("Error: failed to evaluate lhs of operator\n");
          return false;
        }

        // TODO: When generating code,
        //   1. value_stack should have the lhs or dst of the final return value at the top.
        //   2. implement a prepare_call function that evaluates parameters and allocates temporaries.
        //      prepare_call will also allocate the return value.
        //      All temporaries must be allocated before the return value and parameters.
        //      [optimize] If a temporary can be forwarded as a parameter, it should be allocated in the correct place.
        //      [optimize] For inline calls, if the top of the value stack already has a valid candidate for the return value, use that.

        if (!(prepare_call(ast, program, statement.right)
          && generate_call(ast, program)))
          return false;

        break;
      }
      default: {
        auto callableSymbolIndex = program->meta.statement_info[statementId].symbol_index;
        if (!callableSymbolIndex.has_value()) {
          printf("Error: no suitable binary operator\n");
          return false;
        }

        if (!(generate_code(ast, program, statement.right.value())
          && generate_code(ast, program, statement.left.value())))
          return false;
        auto lhs = program->pop_value();
        auto rhs = program->pop_value();

        auto callableSymbol = program->meta.symbols[callableSymbolIndex.value()];
        program_builder::value function;
        function.type_index   = program->meta.get_symbol_type(callableSymbolIndex.value());
        function.symbol_index = callableSymbolIndex.value();
        function.identifier   = expr::get_operator_identifer(statement.type_name);

        auto ret = program->allocate_temporary_value(program->meta.return_type_of(function.type_index.value()).value());
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

    bool generate_code(ast const & ast, program_builder * program, expr::function_declaration const & statement, size_t statementId) {
      const auto &statementMeta = program->meta.statement_info[statementId];
      const auto symbolIndex = statementMeta.symbol_index;
      if (!symbolIndex.has_value()) {
        // TODO: Report error. Declaration does not have a valid symbol
        printf("Error: No symbol associated with function declaration\n");
        return false;
      }

      if (statement.body.has_value() && (statement.flags & symbol_flags::inline_) == symbol_flags::none) {
        program->begin_function(symbolIndex.value());
        program->push_return_pointer();
        program->push_frame_pointer();
        program->move(vm::register_names::fp, vm::register_names::sp);

        program->begin_scope();

        size_t rootScope = program->scopes.size();
        program->push_return_handler([rootScope](auto* program) {
          program->emit_scope_cleanup(rootScope);
          program->jump_relative(0);
          program->set_instruction_tag(program_builder::instruction_tag::return_jmp);
        });

        auto &func = program->current_function();
        int64_t nextArgOffset = -(int64_t)func.args_size - program_builder::function::CallLinkStorageSize; // Frame pointer + return pointer

        for (auto argId : statement.arguments) {
          const auto & decl = ast.get<expr::variable_declaration>(argId);
          const auto & argSymbol = program->meta.statement_info[argId].symbol_index;
          if (!argSymbol.has_value()) {
            // TODO: Report error. Argument has unknown type
            printf("Error: Unknown argument type\n");
            return false;
          }

          program_builder::value val;
          val.symbol_index = argSymbol;
          val.type_index = program->meta.symbols[argSymbol.value()].type;
          val.indirect_register_index = (vm::register_index)vm::register_names::fp;
          val.address_offset = nextArgOffset;
          val.identifier = decl.name;
          nextArgOffset += program->meta.get_type_size(val.type_index.value());

          program->add_variable(val);
        }

        if (!generate_code(ast, program, statement.body.value())) {
          // TODO: Report error. Failed to generate code for function.
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
      val.type_index = program->meta.symbols[symbolIndex.value()].type;
      val.identifier = statement.identifier;
      program->add_variable(val);

      unused(ast, program, statement);
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::call_parameter const& statement, size_t statementId) {
      unused(statementId);

      generate_code(ast, program, statement.expression);

      if (statement.next.has_value())
        generate_code(ast, program, statement.next.value());

      return true;
    }

    bool push_argument(ast const& ast, program_builder * program, std::string_view const & name, program_builder::value src, size_t argType, bool isInline) {
      unused(ast, program, src, argType);

      if (isInline) {
        if (src.type_index == argType) {
          auto cpy = src;
          cpy.identifier = name;
          cpy.flags |= program_builder::value_flags::alias;
          program->add_variable(cpy);
          return true;
        }
        else if (program->meta.is_reference_of(argType, src.type_index)) {
          auto receiver = src;
          receiver.identifier = name;
          // Method expects a reference, so treat the value alias with reference semantics
          receiver.flags |= program_builder::value_flags::eval_as_reference;
          receiver.flags |= program_builder::value_flags::alias;
          program->add_variable(receiver);
          return true;
        }
        else {
          auto receiver = program->allocate_temporary_value(argType);
          receiver.identifier = name;
          program->add_variable(receiver);
          return initialize_variable(ast, program, receiver, src);
        }
      }

      auto receiverId = program->allocate_temporary_call_parameter(argType);
      auto receiver = program->get_temporary(receiverId);
      receiver.identifier = name;
      program->add_variable(receiver);

      if (src.type_index == argType) {
        auto unnamedInit = program->find_unnamed_initializer(argType, argType);

        if (!unnamedInit.has_value()) {
          // TODO: Push error. No unnamed initializer that can create a `receiver` from `initializer`
          auto a = program->meta.types[argType];
          auto b = program->meta.types[argType];
          printf("Error: No unnamed initializer that can create a `%.*s` from `%.*s`\n",
            (int)a.identifier.length(), a.identifier.data(),
            (int)b.identifier.length(), b.identifier.data());
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
      }
      else {
        return initialize_variable(ast, program, receiver, src);
      }
    }

    bool prepare_call_parameters_reversed(ast const& tree, program_builder * program, std::optional<size_t> const& id) {
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

      if (!function->type_index.has_value()) {
        // TODO: Push error. No type
        printf("Error: Callable type is undefined\n");
        return false;
      }
      if (!program->meta.is_function(function->type_index)) {
        // TODO: Push error. Not callable
        auto type = program->meta.types[function->type_index.value()];
        printf("Error: %.*s is not callable\n", (int)type.identifier.length(), type.identifier.data());
        return false;
      }

      auto ret = program->allocate_temporary_value(program->meta.return_type_of(function->type_index.value()).value());
      program->push_value(ret);

      if (!prepare_call_parameters_reversed(tree, program, parameters)) {
        printf("Error: Failed to prepare call parameters\n");
        return false;
      }

      program->push_value(function.value());
      return true;
    }

    // bool prepare_operator_call(program_builder * program, expr::operator_type op) {
    //   auto rhs = program->pop_value();
    //   auto lhs = program->pop_value();
    // 
    //   if (!rhs.has_value() || !lhs.has_value()) {
    //     printf("Error: operator must have 2 operands\n");
    //     return false;
    //   }
    // 
    //   auto ret = program->allocate_temporary_value(program->meta.return_type_of(func->type_index.value()).value());
    //   program->push_value(ret);
    //   program->push_value(rhs.value());
    //   program->push_value(lhs.value());
    //   program->push_value(func.value());
    //   return true;
    // }

    /// Generate a call using expressions pushed to the builders result stack.
    bool generate_call(ast const & ast, program_builder * program) {
      std::optional<program_builder::value> function = program->pop_value();

      if (!function.has_value() || !function->type_index.has_value()) {
        // Push error: First expression must be a callable symbol.
        return false;
      }

      std::optional<program_metadata::symbol> callable;
      if (function->symbol_index.has_value())
        callable = program->meta.symbols[function->symbol_index.value()];

      auto & symbolType = program->meta.types[function->type_index.value()];
      
      // Pointer to the actual function definition.
      // Allows us to inline the call if possible
      expr::function_declaration const * func =
        callable.has_value() && callable->declaration_id.has_value()
        ? &ast.get<expr::function_declaration>(callable->declaration_id.value())
        : nullptr;

      type_function const * signature =
        std::holds_alternative<type_function>(symbolType.desc)
        ? &std::get<type_function>(symbolType.desc)
        : nullptr;

      if (signature == nullptr) {
        // Push error: Type is not callable.
        printf("Error: %.*s is not a callable type\n", (int)symbolType.identifier.length(), symbolType.identifier.data());
        return false;
      }

      const size_t prevTemporaryCount = program->scopes.back().temporaries.size();
      const bool inlineCall = func != nullptr && (func->flags & symbol_flags::inline_) == symbol_flags::inline_;

      if (inlineCall) {
        // TODO: Fix me - inlining functions that allocate stack space is probably broken.
        program->begin_scope();
        const size_t rootScope = program->scopes.size();
        const size_t temporaries = program->scopes.back().temporaries.size();

        program->push_return_handler([rootScope](auto* program) {
          program->emit_scope_cleanup(rootScope);
          program->jump_relative(0);
          program->set_instruction_tag(program_builder::instruction_tag::return_jmp);
        });

        for (size_t i = 0; i < signature->arguments.size(); ++i) {
          auto &var  = ast.get<expr::variable_declaration>(func->arguments[i]);
          auto arg = program->pop_value();
          if (!arg.has_value()) {
            return false;
          }
          push_argument(ast, program, var.name, arg.value(), signature->arguments[i], inlineCall);
        }

        program->push_return_value_receiver(program->value_stack.back());
        
        const size_t startInstruction = program->current_function().instructions.size();

        if (func->body.has_value()) {
          if (!generate_code(ast, program, func->body.value())) {
            return false;
          }
        }

        program->pop_return_value_receiver();

        {
          auto &curFunc = program->current_function();
          for (size_t i = startInstruction; i < curFunc.instructions.size(); ++i) {
            auto& op = curFunc.instructions[i];
            auto& tag = curFunc.instruction_tags[i];
            switch (tag) {
            case program_builder::instruction_tag::return_jmp: {
              // Jump to return statement
              assert(op.code == vm::op_code::jump_relative && "invalid op code tagged with instruction_tag::return_jmp");
              op.jump_relative.offset = (curFunc.instructions.size() - i) * sizeof(vm::instruction);
              curFunc.instruction_tags[i] = program_builder::instruction_tag::none; // Clear tag.
              break;
            }
            }
          }
        }

        while (program->scopes.back().temporaries.size() > temporaries)
          program->free_temporary_value();

        program->end_scope();
      }
      else {
        program->begin_scope();

        auto rv = program->allocate_temporary_call_parameter(program->meta.return_type_of(function->type_index).value());

        for (size_t i = 0; i < signature->arguments.size(); ++i) {
          auto arg = program->pop_value();
          if (!arg.has_value()) {
            return false;
          }
          push_argument(ast, program, "", arg.value(), signature->arguments[i], inlineCall);
        }

        program->call(function.value());

        initialize_variable(ast, program, program->value_stack.back(), program->get_temporary(rv));

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

      auto scopeId = program->meta.statement_info[blockId].scope_index;
      if (!scopeId.has_value()) {
        // No associated scope
        return false;
      }

      program->begin_scope();

      const auto &scopeMeta = program->meta.scopes[scopeId.value()];
      const bool isStackFrame = !scopeMeta.parent_function_scope.has_value();
      const bool isInlining = program->current_function().scope_id != scopeId;

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
        auto& func = program->current_function();

        func.return_section_start = func.instructions.size();

        program->free_stack(1);
        program->set_instruction_tag(program_builder::instruction_tag::stack_frame);
      }

      program->end_scope();
      return true;
    }

    bool generate_code(ast const& ast, program_builder* program, expr::byte_code const & code, size_t statementId) {
      unused(ast, statementId);

      return code.callback != nullptr && code.callback(program);
    }

    bool generate_code(ast const & ast, program_builder * program, size_t statementId) {
      bool result = false;
      std::visit([&](auto const & statement) {
        result = generate_code(ast, program, statement, statementId);
      }, ast.statements[statementId]);
      return result;
    }

    /// Evaluate the type index of a ast statement that refers to a type
    std::optional<size_t> evaluate_type_index(ast const & ast, program_metadata * meta, size_t statementId) {
      std::optional<size_t> ret;
      auto& typeId = meta->statement_info[statementId].type_id;
      if (typeId.has_value()) {
        return typeId;
      }

      if (ast.is<expr::type_modifier>(statementId)) {
        expr::type_modifier const & modifier = ast.get<expr::type_modifier>(statementId);
        auto modified = evaluate_type_index(ast, meta, modifier.modified);
        if (!modified.has_value()) {
          return std::nullopt;
        }
        type_modifier mod;
        mod.base = modified.value();
        mod.const_ = modifier.const_;
        mod.reference = modifier.reference;

        type t;
        t.desc = mod;
        t.identifier = get_type_name(ast, statementId).value();
        typeId = meta->add_type(t);
      }

      if (ast.is<expr::type_fn>(statementId)) {
        expr::type_fn const & fn = ast.get<expr::type_fn>(statementId);

        auto returnType = evaluate_type_index(ast, meta, fn.return_type);
        if (!returnType.has_value()) {
          return std::nullopt;
        }

        type_function desc;
        desc.return_type = returnType.value();
        for (auto const & arg : fn.argument_list) {
          auto argType = evaluate_type_index(ast, meta, arg);
          if (!argType.has_value()) {
            // TODO: Push error. Unable to evaluate argument type at index
            return std::nullopt;
          }
          desc.arguments.push_back(argType.value());
        }
        desc.func_type = fn.func_type;

        type t;
        t.desc = desc;
        t.identifier = get_type_name(ast, statementId).value();

        typeId = meta->add_type(t);
      }

      if (ast.is<expr::class_decl>(statementId)) {
        // Parse class definition

      }

      if (!typeId.has_value()) {
        typeId = meta->get_type_index(ast, statementId);
      }

      return typeId;
    }

    bool evaluate_type_names(ast const & ast, program_metadata * meta) {
      for (size_t i = 0; i < ast.statements.size(); ++i) {
        evaluate_type_index(ast, meta, i);
      }
      return true;
    }

    bool evaluate_types(ast const & ast, program_metadata * meta) {
      return evaluate_type_names(ast, meta);
    }

    struct symbol_eval_context
    {
      size_t scope_id = 0;
      bool is_call = false;
      std::optional<size_t> call_parameter_list;
    };

    bool evaluate_symbols(ast const& ast, program_metadata* meta, size_t id, symbol_eval_context const & ctx);

    template<typename T>
    bool evaluate_statement_symbols(ast const& ast, program_metadata * meta, size_t id, T const & other, symbol_eval_context const & ctx) {
      unused(other);

      bool result = true;
      visit_sub_expressions(ast, id, [&](size_t childId) {
        result = evaluate_symbols(ast, meta, childId, ctx);
      });
      return result;
    }

    bool evaluate_statement_symbols(ast const & ast, program_metadata * meta, size_t id, expr::identifier const & identifier, symbol_eval_context const & ctx) {
      auto& statementMeta = meta->statement_info[id];
      if (ctx.is_call) {
        statementMeta.symbol_index = meta->search_for_callable_symbol_index(ctx.scope_id, identifier.name, ast, ctx.call_parameter_list);
      }
      else {
        statementMeta.symbol_index = meta->search_for_symbol_index(ctx.scope_id, identifier.name);
      }

      if (!statementMeta.symbol_index.has_value()) {
        printf("Error: Unable to resolve identifier '%.*s'\n", (int)identifier.name.length(), identifier.name.data());
        return false;
      }

      statementMeta.type_id = meta->get_symbol_type(statementMeta.symbol_index.value());
      return statementMeta.symbol_index.has_value();
    }

    bool evaluate_literal_symbols(ast const& ast, program_metadata* meta, size_t id, bool value, symbol_eval_context const & ctx) {
      unused(ast, value, ctx);
      meta->statement_info[id].type_id = meta->get_type_index(get_primitive_type_name(type_primitive::bool_));
      return true;
    }

    bool evaluate_literal_symbols(ast const& ast, program_metadata* meta, size_t id, int64_t value, symbol_eval_context const & ctx) {
      unused(ast, value, ctx);
      meta->statement_info[id].type_id = meta->get_type_index(get_primitive_type_name(type_primitive::int64));
      return true;
    }

    bool evaluate_literal_symbols(ast const& ast, program_metadata* meta, size_t id, double value, symbol_eval_context const & ctx) {
      unused(ast, value, ctx);
      meta->statement_info[id].type_id = meta->get_type_index(get_primitive_type_name(type_primitive::float64));
      return true;
    }

    bool evaluate_literal_symbols(ast const& ast, program_metadata* meta, size_t id, std::string_view const & value, symbol_eval_context const& ctx) {
      unused(ast, meta, id, value, ctx);
      assert(false && "Not implemented");
      // TODO: Implement strings
      // meta->statement_info[id].type_id = meta->get_type_index(get_primitive_type_name(type_primitive::));
      return true;
    }

    bool evaluate_statement_symbols(ast const& ast, program_metadata* meta, size_t id, expr::literal const & decl, symbol_eval_context const& ctx) {
      return std::visit([&](auto&& o) {
        return evaluate_literal_symbols(ast, meta, id, o, ctx);
      }, decl.value);
    }

    bool evaluate_statement_symbols(ast const& ast, program_metadata* meta, size_t id, expr::call_parameter const & param, symbol_eval_context const & ctx) {
      unused(id);
      return evaluate_symbols(ast, meta, param.expression, ctx)
        && (!param.next.has_value() || evaluate_symbols(ast, meta, param.next.value(), ctx));
    }

    bool evaluate_statement_symbols(ast const & ast, program_metadata * meta, size_t id, expr::binary_operator const & op, symbol_eval_context const & ctx) {
      unused(id);
      switch (op.type_name) {
      case expr::operator_type::call: {
        assert(op.left.has_value());
        if (op.right.has_value() && !evaluate_symbols(ast, meta, op.right.value(), ctx))
          return false;

        symbol_eval_context callCtx = ctx;
        callCtx.is_call = true;
        callCtx.call_parameter_list = op.right;
        if (!evaluate_symbols(ast, meta, op.left.value(), callCtx)) {
          return false;
        }

        meta->statement_info[id].type_id = meta->return_type_of(meta->statement_info[op.left.value()].type_id);
        break;
      }
      default: {
        assert(op.left.has_value());
        assert(op.right.has_value());
        if (!(evaluate_symbols(ast, meta, op.left.value(), ctx) &&
          evaluate_symbols(ast, meta, op.right.value(), ctx))) {
          printf("Error: failed to evaluate symbols for binary operator operands\n");
          return false;
        }

        const auto lhsType = meta->statement_info[op.left.value()].type_id;
        const auto rhsType = meta->statement_info[op.right.value()].type_id;
        if (!(lhsType.has_value() && rhsType.has_value())) {
          printf("Error: type of binary operator operands could not be determined\n");
          return false;
        }
        const auto symbol = meta->search_for_operator_symbol_index(ctx.scope_id, op.type_name, lhsType.value(), rhsType.value());
        if (!symbol.has_value()) {
          const auto opName = expr::get_operator_identifer(op.type_name);
          const auto lhsName = meta->types[lhsType.value()].identifier;
          const auto rhsName = meta->types[rhsType.value()].identifier;
          
          printf("Error: no suitable binary operator: op='%.*s', lhs='%.*s', rhs='%.*s'\n",
            (int)opName.length(), opName.data(),
            (int)lhsName.length(), lhsName.data(),
            (int)rhsName.length(), rhsName.data()
          );

          return false;
        }

        meta->statement_info[id].symbol_index = symbol;
        meta->statement_info[id].type_id       = meta->return_type_of(meta->symbols[symbol.value()].type);
        break;
      }
      }
      return true;
    }

    bool evaluate_statement_symbols(ast const & ast, program_metadata * meta, size_t id, expr::block const & block, symbol_eval_context const & ctx) {
      auto & statementInfo = meta->statement_info[id];
      size_t thisBlockScopeId = 0;
      symbol_eval_context thisBlockCtx = ctx;
      if (statementInfo.scope_index.has_value()) {
        // Already have a scope index.
        // This is the root scope of a function declaration.
        thisBlockCtx.scope_id = statementInfo.scope_index.value();
      } else {
        thisBlockCtx.scope_id = meta->new_scope(ctx.scope_id);
        statementInfo.scope_index = thisBlockScopeId;

        program_metadata::scope & newScope = meta->scopes[thisBlockScopeId];
        if (newScope.parent.has_value()) {
          program_metadata::scope& parentScope = meta->scopes[newScope.parent.value()];
          newScope.parent_function_scope = parentScope.parent_function_scope;
        }
        newScope.prefix = adder::format("%s/%s/", meta->scopes[ctx.scope_id].prefix.c_str(), block.scope_name.c_str());
      }

      for (auto & statement : block.statements) {
        if (!evaluate_symbols(ast, meta, statement, thisBlockCtx)) {
          return false;
        }
      }

      return true;
    }

    bool evaluate_statement_symbols(ast const & ast, program_metadata * meta, size_t id, expr::function_declaration const & decl, symbol_eval_context const & ctx) {
      program_metadata::symbol symbol;
      symbol.name           = decl.identifier;
      symbol.scope_id       = ctx.scope_id;
      symbol.flags          = decl.flags | symbol_flags::function;
      symbol.declaration_id = id;
      if (decl.type.has_value()) {
        const auto typeIndex = evaluate_type_index(ast, meta, decl.type.value());
        if (!typeIndex.has_value()) {
          // TODO: Error. Expected a type name
          return false;
        }
        symbol.type = typeIndex.value();
      }
      else {
        // TODO: Function type is unknown
        return false;
      }

      const std::string name = std::string(/*decl.identifier.empty() ? adder::format("__unnamed_fn_%lld", ast.id_of(&decl).value()) :*/decl.identifier);
      symbol.full_identifier = name;
      symbol.full_identifier = adder::format(
        "%s%s",
        meta->scopes[ctx.scope_id].prefix.c_str(),
        get_symbol_name(meta->types[symbol.type].identifier, name).c_str());

      const auto symbol_index = meta->add_symbol(symbol);
      if (!symbol_index.has_value()) {
        // TODO: Throw error. Probably need to get the failure reason from add_symbol.
        return false;
      }

      meta->statement_info[id].symbol_index = symbol_index;

      if (decl.body.has_value()) {
        symbol_eval_context thisBlockCtx = ctx;
        thisBlockCtx.scope_id = meta->new_scope(ctx.scope_id);
        meta->statement_info[id].scope_index = thisBlockCtx.scope_id;
        meta->statement_info[decl.body.value()].scope_index = thisBlockCtx.scope_id;

        {
          program_metadata::scope& functionScope = meta->scopes[thisBlockCtx.scope_id];
          functionScope.parent_function_scope = std::nullopt;
          functionScope.prefix = adder::format("%.*s/",
            symbol.full_identifier.length(),
            symbol.full_identifier.data()
          );
        }
        meta->symbols[symbol_index.value()].function_root_scope_id = thisBlockCtx.scope_id;

        for (const size_t statement : decl.arguments) {
          // TODO: Should arguments be part of the "block" statement?
          if (!evaluate_symbols(ast, meta, statement, thisBlockCtx)) {
            return false;
          }
        }

        evaluate_symbols(ast, meta, decl.body.value(), thisBlockCtx);
      }

      return true;
    }

    bool evaluate_statement_symbols(ast const & ast, program_metadata * meta, size_t id, expr::variable_declaration const & decl, symbol_eval_context const & ctx) {
      program_metadata::symbol symbol;
      symbol.name           = decl.name;
      symbol.scope_id       = ctx.scope_id;
      symbol.declaration_id = id;
      symbol.flags          = decl.flags;

      if (decl.type.has_value()) {
        const auto typeIndex = evaluate_type_index(ast, meta, decl.type.value());
        if (!typeIndex.has_value()) {
          // TODO: Error. Expected a type name
          return false;
        }
        symbol.type = typeIndex.value();
      }
      else if (decl.initializer.has_value()) {
        symbol.type = eval_decltype(ast, meta, decl.initializer.value());
      }
      else {
        // TODO: Error. Unable to infer type. No initializer statement.
        return false;
      }

      if (symbol.type == meta->get_type_index(get_primitive_type_name(type_primitive::void_))) {
        // TODO: Error. Symbol does not have a valid type.
        return false;
      }

      symbol.full_identifier = adder::format(
        "%s%s",
        meta->scopes[ctx.scope_id].prefix.c_str(),
        get_symbol_name(meta->types[symbol.type].identifier, symbol.name).c_str());

      auto symbol_index = meta->add_symbol(symbol);
      if (!symbol_index.has_value()) {
        // TODO: Throw error. Probably need to get the failure reason from add_symbol.
        return false;
      }

      if (decl.initializer.has_value()) {
        evaluate_symbols(ast, meta, decl.initializer.value(), ctx);
      }

      meta->statement_info[id].symbol_index = symbol_index;
      meta->statement_info[id].type_id      = symbol.type;
      return true;
    }

    bool evaluate_symbols(ast const & ast, program_metadata * meta, size_t id, symbol_eval_context const & ctx) {
      return std::visit(
        [&](auto&& s) {
          return evaluate_statement_symbols(ast, meta, id, s, ctx);
        },
        ast.statements[id]
      );
    }

    bool evaluate_symbols(ast const & ast, program_metadata * meta) {
      meta->symbols.clear();
      meta->scopes.clear();
      meta->scopes.emplace_back(); // Global scope.

      meta->statement_info.clear();
      meta->statement_info.resize(ast.statements.size());

      expr::block const & top = ast.get<expr::block>(ast.statements.size() - 1);
      symbol_eval_context ctx;
      ctx.scope_id = 0;
      for (size_t statementId : top.statements) {
        if (!evaluate_symbols(ast, meta, statementId, ctx)) {
          return false;
        }
      }
      return true;
    }

    std::optional<program> generate_code(ast const & ast) {
      program_builder ret;

      ret.meta.statement_info.resize(ast.statements.size());

      if (!evaluate_symbols(ast, &ret.meta)) {
        return std::nullopt;
      }

      auto moduleInit = ret.meta.find_symbol("()=>void:$module_init");
      assert(moduleInit.has_value() && "$module_init was not declared");
      ret.meta.symbols[moduleInit.value()].function_root_scope_id = 0;
      ret.begin_function(moduleInit.value());
      ret.begin_scope();

      // Push frame for initializer func
      ret.push_return_pointer();
      ret.push_frame_pointer();
      ret.move(vm::register_names::fp, vm::register_names::sp);

      expr::block const & top = ast.get<expr::block>(ast.statements.size() - 1);
      for (size_t statementId : top.statements) {
        if (!generate_code(ast, &ret, statementId)) {
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

  std::optional<program> compile(std::string const & source) {
    compiler::lexer::token_parser tokenizer(source);
    compiler::ast ast = compiler::parse(&tokenizer);
    if (!tokenizer.ok()) {
      for (auto & error : tokenizer.errors()) {
        printf("Error: %s\n", error.c_str());
      }
      return std::nullopt;
    }

    return generate_code(ast);
  }
}
