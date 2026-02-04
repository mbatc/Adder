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
    bool generate_call(ast const& ast, program_builder* program, size_t startResult);
    bool generate_copy(ast const& ast, program_builder* program, size_t statementId) {
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
      program->push_result(result);
      return true;
    }

    bool generate_literal_code(ast const & ast, program_builder* program, int64_t value) {
      unused(ast);

      program_builder::value result;
      result.constant.emplace(0);
      std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_index = program->meta.get_type_index("int64");
      program->push_result(result);
      return true;
    }

    bool generate_literal_code(ast const & ast, program_builder* program, double value) {
      unused(ast);

      program_builder::value result;
      result.constant.emplace(0);
      std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_index = program->meta.get_type_index("float64");
      program->push_result(result);
      return true;
    }

    bool generate_literal_code(ast const & ast, program_builder * program, std::string_view const & value) {
      unused(ast, value);

      program_builder::value result;
      // result.constant.emplace(0);
      // result.symbol = program->push_data_symbol(value.data(), value.length());
      // std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_index = program->meta.get_type_index("char[]");
      program->push_result(result);
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

      program->push_result(value.value());
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

    bool initialize_variable(ast const & ast, program_builder * program, program_builder::value receiver, program_builder::value initializer) {
      unused(ast, program, receiver, initializer);
      return false;
      // if (!receiver.symbol_index.has_value()) {
      //   return false;
      // }
      // 
      // std::optional<size_t> initializerType = initializer.type_index;
      // if (initializer.symbol_index.has_value())
      //   initializerType = program->meta.symbols[initializer.symbol_index.has_value()].type;
      // 
      // if (!initializerType.has_value()) {
      //   return false;
      // }
      // 
      // if (!receiver.type_index.has_value())
      //   receiver.type_index = program->meta.symbols[receiver.symbol_index.value()].type;
      // 
      // if (program->meta.is_reference_of(receiver.type_index.value(), initializer.type_index.value())) {
      //   // Explicitly init references to types.
      //   auto addr = program->pin_address_of(initializer);
      //   program->store(addr, receiver);
      //   program->release_register(addr);
      //   return true;
      // }
      // 
      // program_builder::expression_result unnamedInit;
      // unnamedInit.symbol_index = program->meta.find_unnamed_initializer(
      //   program->current_scope_id(),
      //   receiver.type_index.value(),
      //   initializerType.value()
      // );
      // 
      // if (!unnamedInit.symbol_index.has_value()) {
      //   // TODO: Push error. No unnamed initializer that can create a `receiver` from `initializer`
      //   return false;
      // }
      // 
      // size_t start = program->results.size();
      // size_t funcType = program->meta.symbols[unnamedInit.symbol_index.value()].type;
      // auto returnType = program->meta.return_type_of(funcType);
      // if (!returnType.has_value()) {
      //   // TODO: Push error. Not a callable type. Has no return type.
      //   return false;
      // }
      // 
      // program->alloc_temporary_value(returnType.value());
      // program->push_expression_result(unnamedInit);
      // program->push_expression_result(receiver);
      // program->push_expression_result(initializer);
      // return generate_call(ast, program, start);
    }

    bool generate_code(ast const & ast, program_builder * program, expr::variable_declaration const & statement, size_t statementId) {
      unused(statementId);
      std::optional<program_builder::value> initializer;
      if (statement.initializer.has_value()) {
        generate_code(ast, program, statement.initializer.value());
        initializer = program->pop_result();
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

      program->add_identifier(statement.name, receiver);

      if (initializer.has_value())
        initialize_variable(ast, program, receiver, initializer.value());

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

      auto receiver = program->results.back();
      if (!receiver.type_index.has_value()) {
        return false;
      }

      if (!program->meta.is_void(receiver.type_index)) {
        if (statement.expression.has_value()) {
          // TODO: Push error. Unexpected expression for function that returns void.
          return false;
        }
        return true;
      }

      if (!generate_code(ast, program, statement.expression.value())) {
        return false;
      }

      if (!initialize_variable(ast, program, receiver, program->results.back())) {
        return false;
      }

      program->ret();
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::binary_operator const & statement, size_t statementId) {
      unused(ast, program, statement, statementId);

      // Allow `generate_code` to push all matching identifiers to `results`.
      // generate_call can then match the correct overload.
      // size_t overloadsEnd = program->results.size();
      size_t startResult = program->results.size();
      generate_code(ast, program, statement.left.value());
      if (statement.right.has_value())
        generate_code(ast, program, statement.right.value());

      switch (statement.type_name) {
      case expr::operator_type::call:
        generate_call(ast, program, startResult);
        break;
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

      const auto &symbol = program->meta.symbols[symbolIndex.value()];

      if (statement.body.has_value()) {
        const size_t returnType = program->meta.return_type_of(symbol.type).value();

        program->begin_function(symbolIndex.value());

        program->push_return_handler([](auto* program) {
          // TODO: Need to end all nested scopes in the function and cleanup variables
          program->end_scope();
          program->ret();
        });

        program->begin_scope();

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
          program->add_identifier(decl.name, val);
        }
        program->allocate_value(returnType);

        if (!generate_code(ast, program, statement.body.value())) {
          // TODO: Report error. Failed to generate code for function.
          return false;
        }

        program->end_scope();
        program->ret();
        program->pop_return_handler();
        program->end_function();
      }

      program_builder::value val;
      val.symbol_index = symbolIndex;
      val.type_index = program->meta.symbols[symbolIndex.value()].type;
      program->add_identifier(statement.identifier, val);

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
          program->add_identifier(name, src);
        }
        else {
          auto receiver = program->allocate_value(argType);
          program->add_identifier(name, receiver);
          return initialize_variable(ast, program, receiver, src);
        }
      }
      else {
        auto receiver = program->allocate_value(argType);
        program->add_identifier(name, receiver);

        if (src.type_index == argType) {
          program_builder::value unnamedInit;
          unnamedInit.symbol_index = program->meta.find_unnamed_initializer(program->current_scope_id(), argType, argType);

          if (!unnamedInit.symbol_index.has_value()) {
            // TODO: Push error. No unnamed initializer that can create a `receiver` from `initializer`
            return false;
          }

          size_t start = program->results.size();
          program->push_result(unnamedInit);
          program->push_result(receiver);
          program->push_result(src);
          return generate_call(ast, program, start);
        }
        else {
          return initialize_variable(ast, program, receiver, src);
        }
      }
      return false;
    }

    /// Generate a call using expressions pushed to the builders result stack.
    bool generate_call(ast const & ast, program_builder * program, size_t startResult) {
      program_builder::value function = program->results[startResult];

      if (!function.symbol_index.has_value()) {
        // Push error: First expression must be a callable symbol.
        return false;
      }

      // Push parameters to the stack
      auto & callable   = program->meta.symbols[function.symbol_index.value()];
      auto & symbolType = program->meta.types[callable.type];

      // Pointer to the actual function definition.
      // Allows us to inline the call if possible
      type_function_decl * func =
        std::holds_alternative<type_function_decl>(symbolType.desc)
          ? &std::get<type_function_decl>(symbolType.desc)
          : nullptr;

      type_function * signature =
        std::holds_alternative<type_function>(symbolType.desc)
        ? &std::get<type_function>(symbolType.desc)
        : nullptr;

      if (signature == nullptr && func == nullptr) {
        // Push error: Type is not callable.
        return false;
      }
      if (signature == nullptr && func != nullptr) {
        auto & funcType = program->meta.types[func->type];
        if (!std::holds_alternative<type_function>(funcType.desc)) {
          // Push error: Function declaration does not have a valid type.
          return false;
        }
        signature = &std::get<type_function>(funcType.desc);
      }

      if (signature->arguments.size() != program->results.size() - startResult - 1) {
        // Push error: Invalid argument count
        return false;
      }

      const bool inlineCall = func != nullptr &&
        (callable.flags & symbol_flags::inline_) == symbol_flags::inline_ &&
        ast.get<expr::function_declaration>(func->function_id).body.has_value();

      // TODO: Eval function parameters.

      // TODO: This needs to happen before any parameter expressions are evaluated.
      //       They might allocate temporary storage which will be freed before this temporary.
      program_builder::value returnResult = program->allocate_value(signature->return_type);

      if (inlineCall) {
        program->begin_scope();

        size_t startScopeIndex = program->scopes.size();
        program->push_return_handler([startScopeIndex](auto* program) {
          // TODO: Need to end all nested scopes in the function and cleanup variables
          while (program->scopes.size() > startScopeIndex) {
            program->end_scope();
          }
          program->jump_relative(0); // TODO: Replace with bytes until end of function
        });

        for (size_t i = 0; i < signature->arguments.size(); ++i) {
          int64_t resultIdx = startResult + i + 1;
          auto &decl = ast.get<expr::function_declaration>(func->function_id);
          auto &var  = ast.get<expr::variable_declaration>(decl.arguments[i]);
          push_argument(ast, program, var.name, program->results[resultIdx], signature->arguments[i], inlineCall);
        }

        auto &decl = ast.get<expr::function_declaration>(func->function_id);
        if (decl.body.has_value()) {
          if (!generate_code(ast, program, decl.body.value())) {
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
          int64_t resultIdx = startResult + i + 1;
          push_argument(ast, program, "", program->results[resultIdx], signature->arguments[i], inlineCall);
        }

        program->call(callable);
        program->end_scope();

        program->pop_frame_pointer();
        program->pop_return_pointer();
      }

      while (program->results.size() > startResult)
        program->pop_result();

      program->push_result(returnResult);
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::call const& statement, size_t statementId) {
      unused(statementId);

      size_t first = program->results.size();

      generate_code(ast, program, statement.functor);

      if (statement.parameters.has_value())
        generate_code(ast, program, statement.parameters.value());

      generate_call(ast, program, first);

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

      auto scope_id = program->meta.statement_info[blockId].scope_id;
      if (!scope_id.has_value()) {
        // Error: Block is not associated with a scope.
        return false;
      }

      program->begin_scope();
      auto scope_meta = program->meta.scopes[scope_id.value()];

      if (scope_meta.symbols_size > 0) {
        program->alloc_stack(scope_meta.symbols_size);
      }

      for (size_t statementId : scope.statements)
        if (!generate_code(ast, program, statementId))
          return false;

      for (auto symbol_id : scope_meta.symbols) {
        if (program->meta.symbols[symbol_id].is_local()) {
          destroy_symbol(ast, program, symbol_id);
        }
      }

      if (scope_meta.symbols_size > 0) {
        program->free_stack(scope_meta.symbols_size);
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
      const size_t thisBlockScopeId = meta->scopes.size();
      meta->scopes.emplace_back();
      meta->scopes.back().parent = scopeId;
      meta->scopes.back().prefix = adder::format("%s%s>", meta->scopes[scopeId].prefix.c_str(), block.scope_name.c_str());

      for (auto & statement : block.statements) {
        if (!evaluate_symbols(ast, meta, statement, thisBlockScopeId)) {
          return false;
        }
      }

      return true;
    }

    bool evaluate_statement_symbols(ast const& ast, program_metadata * meta, size_t scopeId, expr::function_declaration const & decl) {
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

      const std::string name = std::string(decl.identifier.empty() ? adder::format("__unnamed_fn_%lld", ast.id_of(&decl).value()) : decl.identifier);
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
        if (ast.is<expr::block>(decl.body.value())) {
          const auto& block = ast.get<expr::block>(decl.body.value());
          const size_t thisBlockScopeId = meta->scopes.size();
          meta->scopes.emplace_back();
          meta->scopes.back().parent = scopeId;
          meta->scopes.back().prefix = adder::format("%s%s>", meta->scopes[scopeId].prefix.c_str(), block.scope_name.c_str());

          meta->statement_info[decl.body.value()].parent_scope_id = scopeId;
          meta->statement_info[decl.body.value()].scope_id        = thisBlockScopeId;

          for (const size_t& statement : decl.arguments) {
            // TODO: Should arguments be part of the "block" statement?
            if (!evaluate_symbols(ast, meta, statement, thisBlockScopeId)) {
              return false;
            }
          }

          for (const size_t& statement : block.statements) {
            if (!evaluate_symbols(ast, meta, statement, thisBlockScopeId)) {
              return false;
            }
          }

          meta->statement_info[funcStatementId].scope_id = thisBlockScopeId;
        }
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
        symbol.type = evaluate_statement_return_type(ast, meta, decl.initializer.value());
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
      meta->statement_info[id].parent_scope_id = parentScopeId;

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

    void evaluate_variable_addresses(program_metadata * meta) {
      meta->static_storage_size = 0;
      for (auto & scope : meta->scopes) {
        scope.symbols_size = 0;
        size_t paramsSize = 0;

        for (const auto & index : scope.symbols) {
          auto & symbol = meta->symbols[index];
          const bool isParameter = (symbol.flags & symbol_flags::fn_parameter) != symbol_flags::none;
          const bool isStatic    = (symbol.flags & symbol_flags::static_) != symbol_flags::none;
          const bool isFunction  = (symbol.flags & symbol_flags::function) != symbol_flags::none;
          if (isFunction) {
            continue; // Ignore function declarations for this phase.
          }

          const size_t sz = meta->get_type_size(symbol.type);
          const bool isGlobal = symbol.scope_id == 0;

          if (isParameter) {
            symbol.stack_offset = -(int64_t)(paramsSize + sz);
            paramsSize += sz;
          }
          else if (isStatic || isGlobal) {
            symbol.global_address = meta->static_storage_size;
            meta->static_storage_size += sz;
          }
          else {
            symbol.stack_offset = scope.symbols_size;
            scope.symbols_size += sz;
          }
        }
      }
    }

    program generate_code(ast const & ast) {
      program_builder ret;

      ret.meta.statement_info.resize(ast.statements.size());

      // evaluate_types(ast, &ret.meta);
      evaluate_symbols(ast, &ret.meta);
      evaluate_variable_addresses(&ret.meta);

      // generate_code();

      // generate_function_code(ast);

      // evaluate_function_addresses(&ret.meta);
      
      expr::block const & top = ast.get<expr::block>(ast.statements.size() - 1);
      for (size_t statementId : top.statements) {
        generate_code(ast, &ret, statementId);
      }

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
