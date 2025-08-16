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

      program_builder::expression_result result;
      result.constant.emplace(0);
      std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_index = program->get_type_index("bool");
      program->push_expression_result(result);
      return true;
    }

    bool generate_literal_code(ast const & ast, program_builder* program, int64_t value) {
      unused(ast);

      program_builder::expression_result result;
      result.constant.emplace(0);
      std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_index = program->get_type_index("int64");
      program->push_expression_result(result);
      return true;
    }

    bool generate_literal_code(ast const & ast, program_builder* program, double value) {
      unused(ast);

      program_builder::expression_result result;
      result.constant.emplace(0);
      std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_index = program->get_type_index("float64");
      program->push_expression_result(result);
      return true;
    }

    bool generate_literal_code(ast const & ast, program_builder* program, std::string_view const & value) {
      unused(ast, value);

      program_builder::expression_result result;
      // result.constant.emplace(0);
      // result.symbol = program->push_data_symbol(value.data(), value.length());
      // std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_index = program->get_type_index("char[]");
      program->push_expression_result(result);
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
      std::optional<size_t> symbolIndex = program->lookup_identifier_symbol_index(statement.name);
      if (!symbolIndex.has_value()) {
        // Push Error: Undeclared identifier `statement.name`
        return false;
      }

      program_builder::expression_result result;
      result.symbol_index = symbolIndex;
      program->push_expression_result(result);
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

    bool initialize_variable(ast const& ast, program_builder* program, program_builder::expression_result receiver, program_builder::expression_result initializer) {
      if (!receiver.symbol_index.has_value()) {
        return false;
      }

      std::optional<size_t> initializerType = initializer.type_index;
      if (initializer.symbol_index.has_value())
        initializerType = program->symbols[initializer.symbol_index.has_value()].type_index;

      if (!initializerType.has_value()) {
        return false;
      }

      if (!receiver.type_index.has_value())
        receiver.type_index = program->symbols[receiver.symbol_index.value()].type_index;
      
      if (program->is_reference_of(receiver.type_index.value(), initializer.type_index.value())) {
        // Explicitly init references to types.
        auto addr = program->pin_address_of(initializer);
        program->store(addr, receiver);
        program->release_register(addr);
        return true;
      }

      program_builder::expression_result unnamedInit;
      unnamedInit.symbol_index = program->find_unnamed_initializer(receiver.type_index.value(), initializerType.value());

      if (!unnamedInit.symbol_index.has_value()) {
        // TODO: Push error. No unnamed initializer that can create a `receiver` from `initializer`
        return false;
      }

      size_t start = program->results.size();
      program->push_expression_result(unnamedInit);
      program->push_expression_result(receiver);
      program->push_expression_result(initializer);
      return generate_call(ast, program, start);
    }

    bool generate_code(ast const & ast, program_builder * program, expr::variable_declaration const & statement, size_t statementId) {
      unused(statementId);

      std::optional<program_builder::expression_result> initializer;
      if (statement.initializer.has_value())
      {
        generate_code(ast, program, statement.initializer.value());
        initializer = program->pop_expression_result();

        // auto type  = program->get_type(initializer.type_name);
        // program->pin_result(initializer, type_size(type));
        // program->pin_result(initializer, type_size(type));
      }

      std::optional<size_t> type = statement.type;
      if (!type.has_value()) {
        if (!initializer.has_value() || !initializer->type_index.has_value()) {
          // Push Error: Unable to infer variable type.
          return false;
        }

        type = initializer->type_index;
      }

      const size_t variableType = program->get_type_index(ast, statement.type.value());
      program->push_variable(statement.name, variableType, statement.flags);

      program_builder::expression_result receiver = {
        std::nullopt,
        std::nullopt,
        program->lookup_identifier_symbol_index(statement.name),
        variableType
      };

      if (initializer.has_value())
        initialize_variable(ast, program, receiver, initializer.value());

      return true;
    }

    //bool generate_init_code(ast const & ast, program_builder * program, program_builder::expression_result target, type_primitive targetType, program_builder::expression_result value, type_primitive valueType) {
    //  unused(ast);

    //  switch (targetType) {
    //  case type_primitive::int64: {
    //    vm::register_index dst = program->pin_result(target);
    //    vm::register_index src = program->pin_result(value);
    //    vm::instruction str;
    //    str.code = vm::op_code::store;
    //    str.store.dst_addr = dst;
    //    str.store.src = src;
    //    program->add_instruction(str);
    //    program->release_register(dst);
    //    program->release_register(src);
    //  } break;
    //  }
    //  return true;
    //}

    //bool generate_init_code(ast const & ast, program_builder * program, program_builder::expression_result target, type_class targetType, program_builder::expression_result value, type_class valueType) {
    //  unused(ast, program, target, targetType, value, valueType);
    //  return true;
    //}

    //bool generate_init_code(ast const & ast, program_builder * program, program_builder::expression_result target, type_primitive targetType, program_builder::expression_result value, type_class valueType) {
    //  unused(ast, program, target, targetType, value, valueType);
    //  return true;
    //}

    //bool generate_init_code(ast const & ast, program_builder * program, program_builder::expression_result target, type_class targetType, program_builder::expression_result value, type_primitive valueType) {
    //  unused(ast, program, target, targetType, value, valueType);
    //  return true;
    //}

    //template<typename T, typename U>
    //bool generate_init_code(ast const & ast, program_builder * program, program_builder::expression_result target, T targetType, program_builder::expression_result value, U valueType) {
    //  unused(ast, program, target, targetType, value, valueType);
    //  return false;
    //}

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
      unused(ast, program, statement, statementId);
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::binary_operator const & statement, size_t statementId) {
      unused(ast, program, statement, statementId);

      size_t startResult = program->results.size();
      generate_code(ast, program, statement.left.value());
      if (statement.right.has_value())
        generate_code(ast, program, statement.right.value());

      switch (statement.type_name) {
      case expr::operator_type::call:
        generate_call(ast, program, startResult);
      default:
        return false;
      }

      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::function_declaration const & statement, size_t statementId) {
      bool isInitializer = (statement.flags & symbol_flags::initializer) == symbol_flags::initializer;
      auto typeName = get_type_name(ast, statement.type.value());
      if (!typeName.has_value()) {
        // Push error: Invalid function
        return false;
      }

      program_builder::symbol symbol;
      symbol.flags      = statement.flags;
      symbol.name       = adder::format("%s:%s:%.*s", isInitializer ? "init" : "fn", typeName.value().c_str(), statement.identifier.length(), statement.identifier.data());
      symbol.type_index = program->add_function_type(ast, statement, statementId);

      if (statement.body.has_value())
      {
        program->push_symbol_prefix(symbol.name);
        program->scopes.emplace_back();

        // size_t start = program->code.size();
        symbol.address = std::nullopt;
        for (auto argId : statement.arguments) {
          auto & arg = ast.get<expr::variable_declaration>(argId);
          program->push_fn_parameter(arg.name, program->get_type_index(ast, arg.type.value()), arg.flags);
        }

        symbol.function.emplace();
        symbol.function->instruction_offset = program->code.size();
        if (!generate_code(ast, program, statement.body.value()))
          return false;
        program->pop_scope();
        program->ret();

        symbol.function->instruction_count  = program->code.size() - symbol.function->instruction_offset;

        program->pop_symbol_prefix();
      }

      program->push_identifier(statement.identifier, symbol);

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

    bool push_argument(ast const& ast, program_builder * program, std::string_view const & name, program_builder::expression_result src, size_t argType, bool isInline) {
      unused(ast, program, src, argType);

      if (isInline) {
        if (src.type_index == argType) {
          if (src.symbol_index.has_value()) {
            program_builder::symbol alias = program->symbols[src.symbol_index.value()];
            alias.name   = name;
            alias.flags |= symbol_flags::fn_parameter;
            program->push_symbol(alias);
          }
          else if (src.address.has_value()) {
            program_builder::symbol alias;
            alias.name       = name;
            alias.address    = src.address;
            alias.type_index = src.type_index.value();
            alias.flags      = symbol_flags::fn_parameter;
            program->push_symbol(alias);
          }
          else if (src.constant.has_value()) {
            // Push new variable and store the constant in it
            program->push_variable(name, src.type_index.value(), symbol_flags::const_ | symbol_flags::fn_parameter);
            vm::register_index reg = program->pin_constant(src.constant.value());
            program->store(reg, *program->lookup_identifier_symbol(name));
            program->release_register(reg);
          }
        }
        else {
          program->push_variable(name, argType, symbol_flags::fn_parameter | symbol_flags::const_);
          program_builder::expression_result receiver;
          receiver.symbol_index = program->symbols.size() - 1;
          return initialize_variable(ast, program, receiver, src);
        }
      }
      else {
        program->push_variable(name, argType, symbol_flags::fn_parameter | symbol_flags::const_);
        program_builder::expression_result receiver;
        receiver.symbol_index = program->symbols.size() - 1;
        if (src.type_index == argType) {
          program_builder::expression_result unnamedInit;
          unnamedInit.symbol_index = program->find_unnamed_initializer(argType, argType);

          if (!unnamedInit.symbol_index.has_value()) {
            // TODO: Push error. No unnamed initializer that can create a `receiver` from `initializer`
            return false;
          }

          size_t start = program->results.size();
          program->push_expression_result(unnamedInit);
          program->push_expression_result(receiver);
          program->push_expression_result(src);
          return generate_call(ast, program, start);
        }
        else {
          return initialize_variable(ast, program, receiver, src);
        }
      }
      return false;
    }

    /// Generate a call using expressions pushed to the builders result stack.
    bool generate_call(ast const& ast, program_builder * program, size_t startResult) {
      program_builder::expression_result function = program->results[startResult];

      if (!function.symbol_index.has_value()) {
        // Push error: First expression must be a callable symbol.
        return false;
      }

      // Push parameters to the stack
      auto & callable  = program->symbols[function.symbol_index.value()];
      auto & symbolType = program->types[callable.type_index];

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
        auto & funcType = program->types[func->type];
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

      bool inlineCall = func != nullptr && (callable.flags & symbol_flags::inline_) == symbol_flags::inline_;

      if (inlineCall)
        inlineCall &= ast.get<expr::function_declaration>(func->function_id).body.has_value();

      if (inlineCall) {
        program->push_scope(false);
      }
      else {
        program->push_return_pointer();
        program->push_frame_pointer();
        program->move(vm::register_names::fp, vm::register_names::sp);
        program->push_scope(true);
      }

      for (size_t i = 0; i < signature->arguments.size(); ++i) {
        int64_t resultIdx = startResult + i + 1;
        std::string_view name = "";
        if (inlineCall) {
          auto &decl = ast.get<expr::function_declaration>(func->function_id);
          auto &var  = ast.get<expr::variable_declaration>(decl.arguments[i]);
          name = var.name;
        }
        push_argument(ast, program, name, program->results[resultIdx], signature->arguments[i], inlineCall);
      }

      if (inlineCall) {
        auto &decl = ast.get<expr::function_declaration>(func->function_id);
        if (decl.body.has_value()) {
          if (!generate_code(ast, program, decl.body.value())) {
            return false;
          }
        }

        // TODO: Something with decl.return_type_name;
      }
      else {
        program->call(callable);
      }

      program->pop_scope();

      if (!inlineCall) {
        program->pop_frame_pointer();
        program->pop_return_pointer();
      }

      program->results.resize(startResult);
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

    bool generate_code(ast const & ast, program_builder * program, expr::block const & scope, size_t blockId) {
      unused(blockId);

      program->push_scope(false);

      for (size_t statementId : scope.statements)
        if (!generate_code(ast, program, statementId))
          return false;

      program->pop_scope();
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

    bool evaluate_type_name(ast const & ast, program_builder * program, size_t statementId) {
      if (ast.is<expr::type_modifier>(statementId)) {
        expr::type_modifier const & modifier = ast.get<expr::type_modifier>(statementId);
        evaluate_type_name(ast, program, modifier.modified);

        type_modifier mod;
        mod.base = program->get_type_index(ast, modifier.modified);
        mod.const_ = modifier.const_;
        mod.reference = modifier.reference;

        type t;
        t.desc = mod;
        t.identifier = get_type_name(ast, statementId).value();
        program->add_type(t);
      }

      if (ast.is<expr::type_fn>(statementId)) {
        expr::type_fn const & fn = ast.get<expr::type_fn>(statementId);
        type_function desc;
        if (!evaluate_type_name(ast, program, fn.return_type)) {
          return false;
        }

        desc.return_type = program->get_type_index(ast, fn.return_type);
        for (auto const & arg : fn.argument_list) {
          if (!evaluate_type_name(ast, program, arg)) {
            return false;
          }
          desc.arguments.push_back(program->get_type_index(ast, arg));
        }

        type t;
        t.desc = desc;
        t.identifier = get_type_name(ast, statementId).value();
        program->add_type(t);
      }

      return true;
    }

    bool evaluate_type_names(ast const & ast, program_builder * program) {
      for (size_t i = 0; i < ast.statements.size(); ++i) {
        evaluate_type_name(ast, program, i);
      }
      return true;
    }

    bool evaluate_types(ast const & ast, program_builder * program) {
      // TODO: Add user type definitions
      // for (size_t i = 0; i < ast.statements.size(); ++i) {
      // }

      return evaluate_type_names(ast, program);
    }

    program generate_code(ast const & ast) {
      program_builder ret;

      evaluate_types(ast, &ret);

      // TODO: First parse: Add top level symbols

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
