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
  // Compiler implementation
  namespace compiler {
    size_t eval_decltype(ast const& ast, program_metadata * meta, size_t statementId);
    size_t eval_decltype_impl(ast const& ast, program_metadata * meta, size_t statementId, ...) {
      auto metaTypeId = meta->statement_info[statementId].type_id;
      return metaTypeId.value_or(meta->get_type_index(get_primitive_type_name(type_primitive::void_)));
    }

    size_t eval_decltype_impl(ast const& ast, program_metadata * meta, size_t statementId, expr::binary_operator const & call) {

      meta->is_valid_function_overload();
      call.left;
      call.right;
    }

    size_t eval_decltype(ast const & ast, program_metadata * meta, size_t statementId) {
      return std::visit([=](auto&& o) { return eval_decltype_impl(ast, meta, statementId, o); }, ast.statements[statementId]);
    }

    bool generate_call(ast const & ast, program_builder * program, program_builder::value const & function, size_t argsStartIndex);
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
        [=](auto value) {
          return generate_literal_code(ast, program, value);
        },
        statement.value);
    }

    bool generate_code(ast const & ast, program_builder * program, expr::identifier const & statement, size_t statementId) {
      unused(ast, statement, statementId);
      std::optional<program_builder::value> value = program->find_value_by_identifier(statement.name);

      if (!value.has_value()) {
        // Push Error: Undeclared identifier `statement.name`
        return false;
      }

      program->push_value(value.value());
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::list const & statement, size_t statementId) {
      unused(ast, program, statement, statementId);
      return false;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::type_name const & statement, size_t statementId) {
      unused(ast, program, statement, statementId);
      return false;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::type_fn const & statement, size_t statementId) {
      unused(ast, program, statement, statementId);
      return false;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::type_modifier const & statement, size_t statementId) {
      unused(ast, program, statement, statementId);
      return false;
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
        // TODO: Push error. No unnamed initializer that can create a `receiver` from `initializer`
        return false;
      }

      // program->allocate_temporary_value(returnType.value());
      size_t start = program->value_stack.size();
      program->push_value({}); // Null return value
      program->push_value(receiver);
      program->push_value(initializer);
      bool result = generate_call(ast, program, unnamedInit.value(), start);
      program->pop_value();
      program->pop_value();
      program->pop_value();
      return result;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::variable_declaration const & statement, size_t statementId) {
      std::optional<program_builder::value> initializer;
      if (statement.initializer.has_value()) {
        size_t count = program->value_stack.size();
        generate_code(ast, program, statement.initializer.value());
        assert(count < program->value_stack.size());
        initializer = program->pop_value();
      }

      std::optional<size_t> type = statement.type;
      if (!type.has_value()) {
        if (!initializer.has_value() || !initializer->type_index.has_value()) {
          // Push Error: Unable to infer variable type.
          return false;
        }

        type = initializer->type_index;
      }

      const size_t variableType = program->meta.get_type_index(ast, statement.type.value()).value();
      const size_t symbolIndex  = program->meta.statement_info[statementId].symbol_index.value();

      program_builder::value receiver = {
        std::nullopt,
        std::nullopt,
        symbolIndex,
        variableType
      };
      receiver.identifier = statement.name;

      program->add_variable(receiver);

      if (initializer.has_value()) {
        initialize_variable(ast, program, receiver, initializer.value());
      }

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

      if (!statement.expression.has_value()) {
        return false;
      }

      auto receiver = program->get_return_value();
      if (!receiver.type_index.has_value()) {
        return false;
      }

      if (program->meta.is_void(receiver.type_index)) {
        if (statement.expression.has_value()) {
          // TODO: Push error. Unexpected expression for function that returns void.
          return false;
        }
        return true;
      }

      if (!generate_code(ast, program, statement.expression.value())) {
        return false;
      }

      auto expressionResult = program->pop_value();
      if (!expressionResult.has_value()) {
        return false;
      }

      if (!initialize_variable(ast, program, receiver, expressionResult.value())) {
        return false;
      }

      program->return_with_return_handler();
      return true;
    }

    bool prepare_call(ast const & ast, program_builder * program, program_builder::value const & function, std::optional<size_t> const & parameters) {
      if (parameters.has_value()) {
        auto currentParam = parameters;
        while (currentParam.has_value()) {
          auto param = ast.get<expr::call_parameter>(parameters.value());
          generate_code(ast, program, param.expression);
          program->meta.statement_info[param.expression].type_id;
          currentParam = param.next;
        }
      }

      if (!function.symbol_index.has_value()) {
        // Push error: First expression must be a callable symbol.
        return false;
      }

      // Push parameters to the stack
      auto & callable   = program->meta.symbols[function.symbol_index.value()];
      auto & symbolType = program->meta.types[callable.type];

      // Pointer to the actual function definition.
      // Allows us to inline the call if possible
      expr::function_declaration const * func =
        callable.function_index.has_value()
        ? &ast.get<expr::function_declaration>(program->functions[callable.function_index.value()].declaration_id)
        : nullptr;

      type_function * signature =
        std::holds_alternative<type_function>(symbolType.desc)
        ? &std::get<type_function>(symbolType.desc)
        : nullptr;

      if (signature == nullptr) {
        // Push error: Type is not callable.
        return false;
      }

      size_t returnResultIdx = valuesStartIndex;
      size_t argsStartIndex = valuesStartIndex + 1;
      if (signature->arguments.size() != program->value_stack.size() - argsStartIndex) {
        // Push error: Invalid argument count
        return false;
      }

      const bool inlineCall = func != nullptr &&
        (func->flags & symbol_flags::inline_) == symbol_flags::inline_ && callable.function_index.has_value();

      if (inlineCall) {
        program->begin_scope();

        size_t rootScope = program->scopes.size();
        program->push_return_handler([rootScope](auto* program) {
          for (size_t scope = program->scopes.size() - 1; scope >= rootScope; --scope) {
            for (auto const & value : program->scopes[scope].variables) {
              value;
              // TODO:
              // program->destroy_local_variable(value);
            }
          }

          // Might not need return handlers for inlining. Instead,
          //   1. Find all program_builder::instruction_tag::return_jmp tags after generating inline code
          //   2. Replace address with end of inline function.
          //   3. Reset tag to instruction_tag::none
          program->jump_relative(0);
          program->set_instruction_tag(program_builder::instruction_tag::return_jmp);
          });

        for (size_t i = 0; i < signature->arguments.size(); ++i) {
          int64_t resultIdx = argsStartIndex + i;
          auto &var  = ast.get<expr::variable_declaration>(func->arguments[i]);
          push_argument(ast, program, var.name, program->value_stack[resultIdx], signature->arguments[i], inlineCall);
        }

        if (func->body.has_value()) {
          if (!generate_code(ast, program, func->body.value())) {
            return false;
          }
        }

        program->end_scope();
      }
      else {
        program->push_return_pointer();
        program->push_frame_pointer();
        program->move(vm::register_names::fp, vm::register_names::sp);
        program->begin_scope();

        for (size_t i = 0; i < signature->arguments.size(); ++i) {
          int64_t resultIdx = argsStartIndex + i;
          push_argument(ast, program, "", program->value_stack[resultIdx], signature->arguments[i], inlineCall);
        }

        program->call(function);
        program->end_scope();

        program->pop_frame_pointer();
        program->pop_return_pointer();
      }

      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::binary_operator const & statement, size_t statementId) {
      unused(ast, program, statement, statementId);

      // Allow `generate_code` to push all matching identifiers to `results`.
      // generate_call can then match the correct overload.
      // size_t overloadsEnd = program->results.size();
      generate_code(ast, program, statement.left.value());

      switch (statement.type_name) {
      case expr::operator_type::call: {
        size_t startResult = program->value_stack.size();
        prepare_call(ast, program, program->value_stack.back(), statement.right.value());
        // TODO: When generating code,
        //   1. value_stack should have the lhs or dst of the final return value at the top.
        //   2. implement a prepare_call function that evaluates parameters and allocates temporaries.
        //      prepare_call will also allocate the return value.
        //      All temporaries must be allocated before the return value and parameters.
        //      [optimize] If a temporary can be forwarded as a parameter, it should be allocated in the correct place.
        //      [optimize] For inline calls, if the top of the value stack already has a valid candidate for the return value, use that.
        assert(program->value_stack.size() > startResult);
        generate_call(ast, program, program->value_stack[startResult], startResult + 1);
        break;
      }
      default:
        return false;
      }

      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::function_declaration const & statement, size_t statementId) {
      const auto &statementMeta = program->meta.statement_info[statementId];
      const auto symbolIndex = statementMeta.symbol_index;
      if (!symbolIndex.has_value()) {
        // TODO: Report error. Declaration does not have a valid symbol
        return false;
      }

      if (statement.body.has_value()) {
        program->begin_function(symbolIndex.value(), statementId);

        size_t rootScope = program->scopes.size();
        program->push_return_handler([rootScope](auto* program) {
          for (size_t scope = program->scopes.size() - 1; scope >= rootScope; --scope) {
            for (auto const & value : program->scopes[scope].variables) {
              value;
              // TODO: 
              // program->destroy_local_variable(value);
            }
          }

          // Might not need return handlers for inlining. Instead,
          //   1. Find all program_builder::instruction_tag::return_jmp tags after generating inline code
          //   2. Replace address with end of inline function.
          //   3. Reset tag to instruction_tag::none
          program->jump_relative(0);
          program->set_instruction_tag(program_builder::instruction_tag::return_jmp);
        });

        program->begin_scope();

        auto &func = program->current_function();
        int64_t nextArgOffset = -(int64_t)func.args_size;

        for (auto argId : statement.arguments) {
          const auto & decl = ast.get<expr::variable_declaration>(argId);
          const auto & argSymbol = program->meta.statement_info[argId].symbol_index;
          if (!argSymbol.has_value()) {
            // TODO: Report error. Argument has unknown type
            return false;
          }

          program_builder::value val;
          val.symbol_index = argSymbol;
          val.type_index = program->meta.symbols[argSymbol.value()].type;
          val.stack_frame_offset = nextArgOffset;
          val.identifier = decl.name;
          nextArgOffset += program->meta.get_type_size(val.type_index.value());

          program->add_variable(val);
        }

        if (!generate_code(ast, program, statement.body.value())) {
          // TODO: Report error. Failed to generate code for function.
          return false;
        }

        program->end_scope();
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
          program->add_variable(cpy);
          return true;
        }
        else if (program->meta.is_reference_of(argType, src.type_index)) {
          // Push alias to src value
          // TODO: This might not work quite as I expect yet.
          //       Generator needs to be able to treat `receiver` as a reference type.
          //       e.g. "Load value" loads its address.
          auto receiver = src;
          receiver.identifier = name;
          receiver.flags |= program_builder::value_flags::eval_as_reference;
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

      auto receiver = program->allocate_temporary_value(argType);
      receiver.identifier = name;
      program->add_variable(receiver);

      if (src.type_index == argType) {
        auto unnamedInit = program->find_unnamed_initializer(argType, argType);

        if (!unnamedInit.has_value()) {
          // TODO: Push error. No unnamed initializer that can create a `receiver` from `initializer`
          return false;
        }

        size_t start = program->value_stack.size();
        program->push_value({}); // Void return value
        program->push_value(receiver);
        program->push_value(src);
        bool result = generate_call(ast, program, unnamedInit.value(), start);
        program->pop_value();
        program->pop_value();
        program->pop_value();
        return result;
      }
      else {
        return initialize_variable(ast, program, receiver, src);
      }
    }

    /// Generate a call using expressions pushed to the builders result stack.
    bool generate_call(ast const & ast, program_builder * program, program_builder::value const & function, size_t valuesStartIndex) {
      if (!function.symbol_index.has_value()) {
        // Push error: First expression must be a callable symbol.
        return false;
      }

      // Push parameters to the stack
      auto & callable   = program->meta.symbols[function.symbol_index.value()];
      auto & symbolType = program->meta.types[callable.type];

      // Pointer to the actual function definition.
      // Allows us to inline the call if possible
      expr::function_declaration const * func =
        callable.function_index.has_value()
          ? &ast.get<expr::function_declaration>(program->functions[callable.function_index.value()].declaration_id)
          : nullptr;

      type_function * signature =
        std::holds_alternative<type_function>(symbolType.desc)
        ? &std::get<type_function>(symbolType.desc)
        : nullptr;

      if (signature == nullptr) {
        // Push error: Type is not callable.
        return false;
      }

      size_t returnResultIdx = valuesStartIndex;
      size_t argsStartIndex = valuesStartIndex + 1;
      if (signature->arguments.size() != program->value_stack.size() - argsStartIndex) {
        // Push error: Invalid argument count
        return false;
      }

      const bool inlineCall = func != nullptr &&
        (func->flags & symbol_flags::inline_) == symbol_flags::inline_ && callable.function_index.has_value();

      if (inlineCall) {
        program->begin_scope();

        size_t rootScope = program->scopes.size();
        program->push_return_handler([rootScope](auto* program) {
          for (size_t scope = program->scopes.size() - 1; scope >= rootScope; --scope) {
            for (auto const & value : program->scopes[scope].variables) {
              value;
              // TODO:
              // program->destroy_local_variable(value);
            }
          }

          // Might not need return handlers for inlining. Instead,
          //   1. Find all program_builder::instruction_tag::return_jmp tags after generating inline code
          //   2. Replace address with end of inline function.
          //   3. Reset tag to instruction_tag::none
          program->jump_relative(0);
          program->set_instruction_tag(program_builder::instruction_tag::return_jmp);
        });

        for (size_t i = 0; i < signature->arguments.size(); ++i) {
          int64_t resultIdx = argsStartIndex + i;
          auto &var  = ast.get<expr::variable_declaration>(func->arguments[i]);
          push_argument(ast, program, var.name, program->value_stack[resultIdx], signature->arguments[i], inlineCall);
        }

        if (func->body.has_value()) {
          if (!generate_code(ast, program, func->body.value())) {
            return false;
          }
        }

        program->end_scope();
      }
      else {
        program->push_return_pointer();
        program->push_frame_pointer();
        program->move(vm::register_names::fp, vm::register_names::sp);
        program->begin_scope();

        for (size_t i = 0; i < signature->arguments.size(); ++i) {
          int64_t resultIdx = argsStartIndex + i;
          push_argument(ast, program, "", program->value_stack[resultIdx], signature->arguments[i], inlineCall);
        }

        program->call(function);
        program->end_scope();

        program->pop_frame_pointer();
        program->pop_return_pointer();
      }

      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::conversion const & statement, size_t statementId) {
      unused(ast, program, statement, statementId);
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

      if (isStackFrame) {
        // Is function stack frame
        program->alloc_stack(scopeMeta.max_stack_size + scopeMeta.max_temp_size);
      }

      for (size_t statementId : scope.statements)
        if (!generate_code(ast, program, statementId))
          return false;

      if (isStackFrame) {
        // Mark return section
        auto & func = program->current_function();
        func.return_section_start = func.instructions.size();

        program->free_stack(scopeMeta.max_stack_size + scopeMeta.max_temp_size);
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
      std::visit([=, &result](auto const & statement) {
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

      // if (ast.is<expr::function_declaration>(statementId)) {
      //   expr::function_declaration const & decl = ast.get<expr::function_declaration>(statementId);
      // 
      //   type_function_decl fn;
      //   fn.allowInline;
      //   fn.function_id = statementId;
      //   fn.type = evaluate_type_index(ast, meta, decl.type.value()).value_or(0);
      // 
      //   if (fn.type == 0) {
      //     // TODO: Log error. Invalid funciton type.
      //     return std::nullopt;
      //   }
      // 
      //   type t;
      //   t.identifier = decl.identifier.empty() ? adder::format("__unnamed_fn_%lld", statementId) : decl.identifier;
      //   t.identifier = adder::format("%s:%s", t.identifier.c_str(), meta->types[fn.type].identifier.c_str());
      //   t.desc = fn;
      // 
      //   typeId = meta->add_type(t);
      // }

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

    template<typename T>
    size_t evaluate_statement_return_type(ast const & ast, program_metadata const * meta, T const & other) {
      unused(ast, other);
      return meta->get_type_index(get_primitive_type_name(type_primitive::void_)).value();
    }

    size_t evaluate_statement_return_type(ast const & ast, program_metadata const * meta, size_t statement) {
      return std::visit([&](auto &&s) {
        return evaluate_statement_return_type(ast, meta, s);
      }, ast.statements[statement]);
    }


    bool evaluate_symbols(ast const& ast, program_metadata* meta, size_t id, size_t scopeId);

    template<typename T>
    bool evaluate_statement_symbols(ast const& ast, program_metadata * meta, size_t scopeId, T const & other) {
      unused(ast, meta, scopeId, other);
      return true;
    }

    bool evaluate_statement_symbols(ast const & ast, program_metadata * meta, size_t scopeId, expr::block const & block) {
      auto & statementInfo = meta->statement_info[ast.id_of(&block).value()];
      size_t thisBlockScopeId = 0;
      if (statementInfo.scope_index.has_value()) {
        // Already have a scope index.
        // This is the root scope of a function declaration.
        thisBlockScopeId = statementInfo.scope_index.value();
      } else {
        thisBlockScopeId = meta->new_scope(scopeId);
        statementInfo.scope_index = thisBlockScopeId;

        program_metadata::scope & newScope = meta->scopes[thisBlockScopeId];
        if (newScope.parent.has_value()) {
          program_metadata::scope& parentScope = meta->scopes[newScope.parent.value()];
          newScope.parent_function_scope = parentScope.parent_function_scope;
        }
        newScope.prefix = adder::format("%s/%s/", meta->scopes[scopeId].prefix.c_str(), block.scope_name.c_str());
      }

      for (auto & statement : block.statements) {
        if (!evaluate_symbols(ast, meta, statement, thisBlockScopeId)) {
          return false;
        }
      }

      return true;
    }

    bool evaluate_statement_symbols(ast const & ast, program_metadata * meta, size_t scopeId, expr::function_declaration const & decl) {
      program_metadata::symbol symbol;
      symbol.name     = decl.identifier;
      symbol.scope_id = scopeId;
      symbol.flags    = symbol_flags::function;

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
        meta->scopes[scopeId].prefix.c_str(),
        get_symbol_name(meta->types[symbol.type].identifier, name).c_str());

      const auto symbol_index = meta->add_symbol(symbol);
      if (!symbol_index.has_value()) {
        // TODO: Throw error. Probably need to get the failure reason from add_symbol.
        return false;
      }

      const size_t funcStatementId = ast.id_of(&decl).value();
      meta->statement_info[funcStatementId].symbol_index = symbol_index;

      if (decl.body.has_value()) {
        const size_t thisBlockScopeId = meta->new_scope(scopeId);
        meta->statement_info[funcStatementId].scope_index = thisBlockScopeId;
        meta->statement_info[decl.body.value()].scope_index = thisBlockScopeId;

        {
          program_metadata::scope& functionScope = meta->scopes[thisBlockScopeId];
          functionScope.parent_function_scope = std::nullopt;
          functionScope.prefix = adder::format("%.*s/",
            symbol.full_identifier.length(),
            symbol.full_identifier.data()
          );
        }
        meta->symbols[symbol_index.value()].function_root_scope_id = thisBlockScopeId;

        for (const size_t statement : decl.arguments) {
          // TODO: Should arguments be part of the "block" statement?
          if (!evaluate_symbols(ast, meta, statement, thisBlockScopeId)) {
            return false;
          }
        }

        evaluate_symbols(ast, meta, decl.body.value(), thisBlockScopeId);
      }

      return true;
    }

    bool evaluate_statement_symbols(ast const & ast, program_metadata * meta, size_t scopeId, expr::variable_declaration const & decl) {
      program_metadata::symbol symbol;
      symbol.scope_id = scopeId;
      symbol.name = decl.name;

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

      symbol.flags = decl.flags;
      symbol.full_identifier = adder::format(
        "%s%s",
        meta->scopes[scopeId].prefix.c_str(),
        get_symbol_name(meta->types[symbol.type].identifier, symbol.name).c_str());

      auto symbol_index = meta->add_symbol(symbol);
      if (!symbol_index.has_value()) {
        // TODO: Throw error. Probably need to get the failure reason from add_symbol.
        return false;
      }

      meta->statement_info[ast.id_of(&decl).value()].symbol_index = symbol_index;
      return true;
    }

    bool evaluate_symbols(ast const & ast, program_metadata * meta, size_t id, size_t parentScopeId) {
      // meta->statement_info[id].parent_scope_id = parentScopeId;
      return std::visit(
        [&](auto&& s) {
          return evaluate_statement_symbols(ast, meta, parentScopeId, s);
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
      for (size_t statementId : top.statements) {
        if (!evaluate_symbols(ast, meta, statementId, 0)) {
          return false;
        }
      }
      return true;
    }

    void evaluate_stack_allocations(program_metadata * meta, const size_t rootId = 0, const size_t allocatedStackSpace = 0) {
      program_metadata::scope & root = meta->scopes[rootId];
      size_t stackSize = root.parent_function_scope.has_value() ? allocatedStackSpace : 0;
      for (size_t symbolId : root.symbols) {
        program_metadata::symbol & symbol = meta->symbols[symbolId];
        if (!symbol.has_local_storage()) {
          continue;
        }

        symbol.stack_offset = stackSize;
        stackSize += meta->get_type_size(symbol.type);
      }

      if (root.parent_function_scope.has_value()) {
        program_metadata::scope & storageScope = meta->scopes[root.parent_function_scope.value()];
        storageScope.max_stack_size = std::max(storageScope.max_stack_size, stackSize);
      }
      else {
        root.max_stack_size = stackSize;
      }

      meta->for_each_child_scope(rootId, [&](size_t childId) {
        evaluate_stack_allocations(meta, childId, stackSize);
      });
    }

    void evaluate_variable_addresses(program_metadata * meta) {
      meta->static_storage_size = 0;
      for (auto & scope : meta->scopes) {
        for (const auto & index : scope.symbols) {
          program_metadata::symbol & symbol = meta->symbols[index];

          const bool isParameter = symbol.is_parameter();
          const bool isStatic    = symbol.is_static();
          const bool isFunction  = symbol.is_function();

          if (isFunction || isParameter) {
            continue; // Ignore function declarations for this phase.
          }

          const size_t sz = meta->get_type_size(symbol.type);
          const bool isGlobal = !symbol.is_global();
          if (isStatic || isGlobal) {
            symbol.global_address = meta->static_storage_size;
            meta->static_storage_size += sz;
          }
        }
      }
    }

    program generate_code(ast const & ast) {
      program_builder ret;

      ret.meta.statement_info.resize(ast.statements.size());

      evaluate_symbols(ast, &ret.meta);
      evaluate_stack_allocations(&ret.meta);
      evaluate_variable_addresses(&ret.meta);

      // generate_code();

      // generate_function_code(ast);

      // evaluate_function_addresses(&ret.meta);
      
      ret.begin_scope();
      expr::block const & top = ast.get<expr::block>(ast.statements.size() - 1);
      for (size_t statementId : top.statements) {
        generate_code(ast, &ret, statementId);
      }
      ret.end_scope();

      return ret.binary();
    }
  }

  program compile(std::string const & source) {
    compiler::lexer::token_parser tokenizer(source);
    compiler::ast ast = compiler::parse(&tokenizer);
    if (!tokenizer.ok()) {
      for (auto & error : tokenizer.errors()) {
        printf("Error: %s\n", error.c_str());
      }
    }

    return generate_code(ast);
  }
}
