#include "compiler.h"
#include "common.h"
#include "vm.h"
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

    bool generate_literal_code(ast const& ast, program_builder* program, bool value) {
      unused(ast);

      program_builder::expression_result result;
      result.constant.emplace(0);
      std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_index = program->get_type_index("bool");
      program->push_expression_result(result);
      return true;
    }

    bool generate_literal_code(ast const& ast, program_builder* program, int64_t value) {
      unused(ast);

      program_builder::expression_result result;
      result.constant.emplace(0);
      std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_index = program->get_type_index("int64");
      program->push_expression_result(result);
      return true;
    }

    bool generate_literal_code(ast const& ast, program_builder* program, double value) {
      unused(ast);

      program_builder::expression_result result;
      result.constant.emplace(0);
      std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_index = program->get_type_index("float64");
      program->push_expression_result(result);
      return true;
    }

    bool generate_literal_code(ast const& ast, program_builder* program, std::string_view const & value) {
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
      std::optional<size_t> symbolIndex = program->find_symbol_index(statement.name);
      if (!symbolIndex.has_value()) {
        // Push Error: Undeclared identifier `statement.name`
        return false;
      }

      program_builder::expression_result result;
      result.symbol_index = symbolIndex;
      program->push_expression_result(result);
      return true;
    }

    bool initialize_variable(ast const & ast, program_builder * program, program_builder::expression_result receiver, program_builder::expression_result initializer) {
      if (receiver.symbol_index.has_value()) {
        // Resolve symbol address
        program_builder::symbol_desc &receiverSymbol = program->symbols[receiver.symbol_index.value()];
        receiver.symbol_index.reset();
        receiver.type_index = receiverSymbol.type_index;
        return initialize_variable(ast, program, receiver, initializer);
      }

      // if (initializer.type_index.has_value()) {
      //   size_t symbolIndex = program->find_initializer(
      //     receiver.type_index.value(),
      //     initializer.type_index.value());
      // }
      
      if (initializer.symbol_index.has_value()) {
        program_builder::symbol_desc symbol = program->symbols[initializer.symbol_index.value()];

        if ((symbol.flags & symbol_flags::initializer) == symbol_flags::none) {
          return false;
        }



        return true;
      }

      if (initializer.type_index.has_value()) {
        std::optional<size_t> symbolIndex = program->find_unnamed_initializer(receiver.type_index.value(), initializer.type_index.value());
        if (!symbolIndex.has_value()) {
          // TODO: Push error. No unnamed initializer that can create a `receiver` from `initializer`
        }

        program_builder::expression_result unnamedInit;
        unnamedInit.symbol_index = symbolIndex;

        size_t start = program->results.size();
        program->push_expression_result(unnamedInit);
        program->push_expression_result(receiver);
        program->push_expression_result(initializer);
        return generate_call(ast, program, start);
      }

      return false;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::variable_declaration const & statement, size_t statementId) {
      unused(statementId);
      
      size_t typeIndex = program->get_type_index(statement.type_name);
      // type const &typeInfo = program->types[typeIndex];

      program->push_variable(statement.name, statement.type_name, statement.flags);

      program_builder::expression_result receiver = {
        std::nullopt,
        std::nullopt,
        program->find_symbol_index(statement.name),
        typeIndex
      };

      if (statement.initializer.has_value())
      {
        generate_code(ast, program, statement.initializer.value());

        auto initializer = program->pop_expression_result();

        initialize_variable(ast, program, receiver, initializer);

        // auto type  = program->get_type(initializer.type_name);
        // program->pin_result(initializer, type_size(type));
        // program->pin_result(initializer, type_size(type));
      }

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

    bool generate_code(ast const & ast, program_builder * program, expr::init const& statement, size_t statementId) {
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

    bool generate_code(ast const & ast, program_builder * program, expr::function_return const& statement, size_t statementId) {
      unused(ast, program, statement, statementId);
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::binary_operator const& statement, size_t statementId) {
      unused(ast, program, statement, statementId);
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::function_declaration const& statement, size_t statementId) {
      program_builder::symbol_desc symbol;
      symbol.flags = statement.flags;
      symbol.identifier = statement.identifier;
      symbol.type_index = program->add_function_type(ast, statement, statementId);

      if (statement.body.has_value())
      {
        program->push_scope();
        // size_t start = program->code.size();
        symbol.address = std::nullopt;
        for (auto argId : statement.arguments) {
          auto & arg = ast.get<expr::variable_declaration>(argId);
          program->push_fn_parameter(arg.name, program->get_type_index(arg.type_name), arg.flags);
        }

        if (!generate_code(ast, program, statement.body.value()))
          return false;

        for (auto argId : statement.arguments) {
          unused(argId);
          program->pop_symbol();
        }

        program->pop_scope();
      }

      program->push_symbol(statement.identifier, symbol);

      statement.signature;

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
            program_builder::symbol_desc alias = program->symbols[src.symbol_index.value()];
            alias.identifier = name;
            alias.flags |= symbol_flags::fn_parameter;
            program->push_symbol(name, alias);
          }
          else if (src.address.has_value()) {
            program_builder::symbol_desc alias;
            alias.identifier = name;
            alias.address    = src.address;
            alias.type_index = src.type_index.value();
            alias.flags      = symbol_flags::fn_parameter;
            program->push_symbol(name, alias);
          }
          else if (src.constant.has_value()) {
            // Push new variable and store the constant in it
            program->push_variable(name, src.type_index.value(), symbol_flags::const_ | symbol_flags::fn_parameter);
            vm::register_index reg = program->pin_constant(src.constant.value());
            program->store(reg, program->symbols.back());
          }
        }
        else {
          // Need to push new symbol and convert into it.
        }
      }
      else {
        if (src.type_index == argType) {
          // Copy argument.
          program->push_variable(name, argType, symbol_flags::fn_parameter | symbol_flags::const_);
          // program->find_unnamed_copy();
          // sgenerate_copy(ast, );
        }
      }
      return false;
    }

    /// Generate a call using expressions pushed to the builders result stack.
    bool generate_call(ast const& ast, program_builder* program, size_t startResult) {
      program_builder::expression_result function = program->results[startResult];

      if (!function.symbol_index.has_value()) {
        // Push error: First expression must be a callable symbol.
        return false;
      }

      // Push parameters to the stack
      auto & callable  = program->symbols[function.symbol_index.value()];
      auto & callableT = program->types[callable.type_index];
      if (!std::holds_alternative<type_function>(callableT.desc)) {
        // Push error: Type is not callable.
        return false;
      }

      auto & func = std::get<type_function>(callableT.desc);
      if (func.arguments.size() != program->results.size() - startResult - 1) {
        // Push error: Invalid argument count
        return false;
      }

      bool inlineCall = func.function_id.has_value() && func.allowInline;

      if (inlineCall)
        inlineCall &= ast.get<expr::function_declaration>(func.function_id.value()).body.has_value();

      for (size_t i = 0; i < func.arguments.size(); ++i) {
        std::string_view name = "";
        if (inlineCall) {
          auto &decl = ast.get<expr::function_declaration>(func.function_id.value());
          auto &body = ast.get<expr::block>(decl.body.value());
          auto &var  = ast.get<expr::variable_declaration>(body.statements[i]);
          name = var.name;
        }
        push_argument(ast, program, name, program->results[i], func.arguments[i], inlineCall);
      }

      if (inlineCall) {
        auto &decl = ast.get<expr::function_declaration>(func.function_id.value());
        if (decl.body.has_value()) {
          if (!generate_code(ast, program, decl.body.value())) {
            return false;
          }
        }

        // TODO: Something with decl.return_type_name;
      }
      else {
        if (callable.address.has_value()) {
          // Insert jmp to address.
        }
        else {
          // Insert relocation to evaluate once the function address is known.
          // callable.flags
        }
      }

      for (size_t i = startResult + 1; i < program->results.size(); ++i) {
        program->pop_variable();
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

    bool generate_code(ast const & ast, program_builder * program, expr::class_decl const& statement, size_t statementId) {
      unused(ast, program, statement, statementId);
      return true;
    }

    bool generate_code(ast const & ast, program_builder * program, expr::block const & scope, size_t blockId) {
      unused(blockId);

      for (size_t statementId : scope.statements)
        if (!generate_code(ast, program, statementId))
          return false;
      return true;
    }

    bool generate_code(ast const& ast, program_builder* program, expr::byte_code const& code, size_t statementId) {
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

    program generate_code(ast const & ast) {
      program_builder ret;
      expr::block const & top = ast.get<expr::block>(ast.statements.size() - 1);
      for (size_t statementId : top.statements) {
        generate_code(ast, &ret, statementId);
      }

      return {}; // ret.binary();
    }
  }

  compiler::program compile(std::string const & source) {
    compiler::lexer::token_parser tokenizer(source);
    compiler::ast ast = compiler::parse(&tokenizer);
    if (!tokenizer.ok()) {
      for (auto & error : tokenizer.errors()) {
        printf("Error: %s\n", error.c_str());
      }
    }

    // evaluate_types(&ast);
    // evaluate_conversions(&ast);

    return generate_code(ast);
  }
}
