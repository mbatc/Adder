#include "parser.h"
#include "ast.h"
#include "ast/builtins.h"

using namespace adder::compiler::expr;

namespace adder {
  namespace compiler {
    struct ast;

    namespace parser {
      namespace rules {
        token_rule::token_rule(std::function<bool(lexer::token_view const &)> const & func)
          : pred(func)
        {}

        token_rule::token_rule(lexer::token_id id) : pred([=](lexer::token_view const & token) { return token.id == id; }) {}
        token_rule::token_rule(lexer::token_class cls) : pred([=](lexer::token_view const & token) { return token.cls == cls; }) {}

        bool token_rule::operator()(lexer::token_view const & token) const {
          return pred(token);
        }

        token_rule or(token_rule const & a, token_rule const & b) {
          return token_rule([=](lexer::token_view const & token) { return a(token) || b(token); });
        }

        token_rule xor(token_rule const & a, token_rule const & b) {
          return token_rule([=](lexer::token_view const & token) { return a(token) ^ b(token); });
        }

        token_rule and(token_rule const & a, token_rule const & b) {
          return token_rule([=](lexer::token_view const & token) { return a(token) && b(token); });
        }
      }

      expr::operator_type get_operator_type(lexer::token_id token) {
        switch (token) {
        case lexer::token_id::assign:        return expr::operator_type::assign;
        case lexer::token_id::equal:         return expr::operator_type::equal;
        case lexer::token_id::not_equal:     return expr::operator_type::not_equal;
        case lexer::token_id::less_equal:    return expr::operator_type::less_equal;
        case lexer::token_id::greater_equal: return expr::operator_type::greater_equal;
        case lexer::token_id::less:          return expr::operator_type::less;
        case lexer::token_id::greater:       return expr::operator_type::greater;
        case lexer::token_id::bang:          return expr::operator_type::bang;
        case lexer::token_id::multiply:      return expr::operator_type::multiply;
        case lexer::token_id::divide:        return expr::operator_type::divide;
        case lexer::token_id::add:           return expr::operator_type::add;
        case lexer::token_id::minus:         return expr::operator_type::minus;
        case lexer::token_id::dot:           return expr::operator_type::dot;
        }
        return expr::operator_type::unknown;
      }

      // Function scope conventions.
      // 1. Allocate all stack variables at top of scope.
      // 2. Pop all stack variables at end of scope.

      // Calling convention
      // 1. Push used registers to stack.
      // 2. Push pc to stack.
      // 3. Push arguments to stack in order specified.
      // 4. Jump to function address/entry point.
      //      Execute function (reference arguments from stack). All registers are unused, so use registers as required.
      //      Push return value to stack.
      // 6. Pop/use return value.
      // 7. Pop all arguments from stack.

      // Wrap a block in the byte code for a function call.
      // r0 reserved for return value.
      // r1-register_count for parameters.
      // std::vector<uint8_t> function_call(/* signature */) {
      //   // Assumes all registers are unused
      //   // Used registers should be pushed to the stack beforehand
      // 
      // }

      // Handle consuming the result of a function
      // std::vector<uint8_t> function_return() {
      // 
      // }

      std::optional<size_t> consume_return(ast * tree, lexer::token_parser * tokenizer) {
        if (!tokenizer->parse(lexer::token_id::return_).ok()) {
          return std::nullopt;
        }

        std::optional<size_t> expression = consume_expression(tree, tokenizer);
        tree->add(function_return{ expression });
        return true;
      }

      std::optional<size_t> consume_block(ast * tree, lexer::token_parser * tokenizer) {
        if (!tokenizer->parse(lexer::token_id::open_brace).ok())
          return std::nullopt;

        block scope;
        scope.scope_name   = std::to_string(tree->statements.size() + 1);
        while (tokenizer->current().id != lexer::token_id::close_brace)
        {
          auto expression = consume_expression(tree, tokenizer);
          if (!expression.has_value())
            break;

          scope.statements.push_back(expression.value());
        }

        if (tokenizer->eof())
          return std::nullopt;

        return tree->add(std::move(scope));
      }

      std::optional<size_t> consume_parameter(ast * tree, lexer::token_parser * tokenizer) {
        return consume_variable_decl(tree, tokenizer, symbol_flags::fn_parameter | symbol_flags::const_, rules::or(lexer::token_id::comma, lexer::token_id::close_paren));
      }

      bool takes_precedence(ast * tree, size_t statement, size_t over) {
        return tree->get_precedence(statement) > tree->get_precedence(over);
      }

      // Assumes newStatement takes precedence over parentId
      bool insert_statement(ast * tree, size_t parentId, size_t newStatementId) {
        if (!tree->is<binary_operator>(parentId)) {
          return false;
        }

        auto& parent = tree->get<binary_operator>(parentId);
        if (parent.right.has_value() && takes_precedence(tree, newStatementId, parent.right.value())) {
          return insert_statement(tree, parent.right.value(), newStatementId);
        }
        else {
          auto oldRight = parent.right;
          parent.right = newStatementId;

          if (oldRight.has_value()) {
            if (tree->is<binary_operator>(newStatementId)) {
              tree->get<binary_operator>(newStatementId).left = oldRight;
            }
            else {
              // oldRight was lost... something bad may happen
              return false;
            }
          }
        }

        return true;
      }

      bool insert_statement(ast * tree, std::optional<size_t> *pRoot, size_t newStatementId) {
        if (!pRoot->has_value()) {
          *pRoot = newStatementId;
          return true;
        }

        if (takes_precedence(tree, pRoot->value(), newStatementId)) {
          auto &op = tree->get<binary_operator>(newStatementId);

          // Make next statement top of tree
          op.left = *pRoot;
          *pRoot = std::move(newStatementId);
          return true;
        }
        else {
          return insert_statement(tree, pRoot->value(), newStatementId);
        }
      }

      std::optional<size_t> consume_call(ast * tree, lexer::token_parser * tokenizer) {
        if (!tokenizer->parse(lexer::token_id::open_paren).ok())
          return std::nullopt;

        binary_operator call;
        call.type_name  = operator_type::call;
        std::optional<size_t> lastParameter;
        while (tokenizer->current().id != lexer::token_id::close_paren) {
          std::optional<size_t> expression = consume_expression(tree, tokenizer, rules::or(lexer::token_id::comma, lexer::token_id::close_paren));
          if (!expression.has_value())
            return std::nullopt;

          call_parameter param;
          param.expression = expression.value();
          size_t paramId = tree->add(param);

          if (lastParameter.has_value()) {
            tree->get<call_parameter>(lastParameter.value()).next = paramId;
          }

          if (!call.right.has_value()) {
            call.right = paramId;
          }

          lastParameter = paramId;

          if (tokenizer->previous().id == lexer::token_id::close_paren)
            break;
        }

        return tree->add(call);
      }

      bool is_callable(ast * tree, size_t expressionId) {
        return !tree->is<binary_operator>(expressionId) || tree->get<binary_operator>(expressionId).type_name == operator_type::call;
      }

      std::optional<size_t> consume_literal(ast* tree, lexer::token_parser* tokenizer) {
        lexer::token_view value;
        if (!tokenizer->parse(lexer::token_class::literal_, &value).ok())
          return std::nullopt;

        switch (value.id) {
        case lexer::token_id::true_:          return tree->add(literal{ true });
        case lexer::token_id::false_:         return tree->add(literal{ false });
        case lexer::token_id::integer:        return tree->add(literal{ std::atoll(std::string(value.name).c_str()) });
        case lexer::token_id::decimal:        return tree->add(literal{ std::atof(std::string(value.name).c_str()) });
        case lexer::token_id::string_literal: return tree->add(literal{ value.name });
        default: break;
        }

        return std::nullopt;
      }

      std::optional<size_t> consume_init_expression(ast* tree, lexer::token_parser* tokenizer) {
        lexer::token_view value;
        if (!tokenizer->parse(lexer::token_id::init).ok())
          return std::nullopt;

        std::optional<size_t> target     = consume_expression(tree, tokenizer, lexer::token_id::as);
        std::optional<size_t> expression = consume_expression(tree, tokenizer);

        if (!target.has_value() || !expression.has_value())
          return std::nullopt;

        init initializer;
        initializer.target = target.value();
        initializer.expression = expression.value();
        return tree->add(initializer);
      }

      std::optional<size_t> consume_expression(ast * tree, lexer::token_parser * tokenizer, rules::token_rule const & terminator) {
        std::optional<size_t> ret;
        lexer::token_view token, previous;

        while (true) {
          token = tokenizer->current();
          previous = tokenizer->previous();

          if (terminator(token))
          {
            tokenizer->next();
            break;
          }

          std::optional<size_t> subExpression;
          switch (token.id)
          {
          case lexer::token_id::open_paren:
            if (get_operator_type(previous.id) == operator_type::unknown) {
              subExpression = consume_call(tree, tokenizer);
            } else {
              tokenizer->next();
              subExpression = consume_expression(tree, tokenizer, lexer::token_id::close_paren);
            }
            break;
          case lexer::token_id::open_brace:
            subExpression = consume_block(tree, tokenizer);
            break;
          case lexer::token_id::identifier:
            subExpression = tree->add(identifier{ token.name });
            tokenizer->next();
            break;
          case lexer::token_id::this_:
            subExpression = tree->add(identifier{ token.name });
            tokenizer->next();
            break;
          case lexer::token_id::return_:
            return consume_return(tree, tokenizer);
          case lexer::token_id::let:
            return consume_let(tree, tokenizer);
          case lexer::token_id::const_:
            return consume_const(tree, tokenizer);
          case lexer::token_id::init:
            return consume_init_expression(tree, tokenizer);
          default: {
            if (token.cls == lexer::token_class::literal_) {
              subExpression = consume_literal(tree, tokenizer);
            }
            else {
              if (!tokenizer->parse(lexer::token_class::operator_, &token).ok())
                return std::nullopt;

              auto operatorType = get_operator_type(token.id);
              binary_operator op;
              op.type_name = operatorType;
              subExpression = tree->add(op);
            }
            break;
          }
          }

          if (!subExpression.has_value())
            return std::nullopt;

          insert_statement(tree, &ret, subExpression.value());
        }

        return ret;
      }

      std::optional<size_t> consume_statement(ast* tree, lexer::token_parser* tokenizer) {
        switch (tokenizer->current().id) {
        case lexer::token_id::fn:         return consume_fn(tree, tokenizer);
        case lexer::token_id::class_:     return consume_class(tree, tokenizer);
        case lexer::token_id::extern_:    return consume_extern(tree, tokenizer);
        case lexer::token_id::open_brace: return consume_block(tree, tokenizer);
          // case lexer::token_id::if_:        return consume_if(tree, tokenizer);
        default:                          return consume_expression(tree, tokenizer);
        }
      }

      bool consume_function_parameter_list(ast * tree, std::vector<size_t> * arguments, lexer::token_parser * tokenizer) {
        while (tokenizer->current().id != lexer::token_id::close_paren) {
          auto paramId = consume_parameter(tree, tokenizer);
          if (!paramId.has_value())
            return false;

          arguments->push_back(paramId.value());

          if (tokenizer->current().id == lexer::token_id::close_paren)
            break;
          if (tokenizer->current().id != lexer::token_id::comma)
            return false;

          tokenizer->next();
        }
        tokenizer->next();
        return true;
      }

      bool consume_function_body(ast* tree, block * body, lexer::token_parser* tokenizer) {
        lexer::token_view token;
        if (!tokenizer->
          parse(rules::or(
            lexer::token_id::semi_colon,
            lexer::token_id::open_brace), &token)
          .ok())
          return false;

        if (token.id == lexer::token_id::open_brace) {
          // Parse function body.
          while (tokenizer->current().id != lexer::token_id::close_brace) {
            std::optional<size_t> expression = consume_expression(tree, tokenizer);
            if (!expression.has_value())
              return false;

            body->statements.push_back(expression.value());
          }

          tokenizer->next();
        }
        return true;
      }

      std::optional<size_t> add_function_declaration(ast* tree, std::string_view identifier, std::string_view returnType, std::vector<size_t> &&arguments, size_t bodyId, symbol_flags flags) {
        std::string signature = "(";
        size_t numParams = 0;
        for (auto & paramId : arguments) {
          if (!tree->is<variable_declaration>(paramId)) {
            // Push Error: Invalid statement in argument list.
            return std::nullopt;
          }

          auto &decl = tree->get<variable_declaration>(paramId);
          if (numParams != 0)
            signature += ",";
          signature += decl.type_name;
          ++numParams;
        }

        if ((flags & symbol_flags::initializer) != symbol_flags::none) {
          signature = "init:" + signature;
        } else {
          signature = "fn:" + signature + std::string(returnType);
        }

        function_declaration function;
        function.identifier = identifier;
        function.arguments = std::move(arguments);
        function.signature = std::string(identifier) + signature;
        function.return_type_name = returnType;
        function.flags = flags;
        function.body = bodyId;

        return tree->add(function);
      }

      std::optional<size_t> consume_function_declaration(ast* tree, lexer::token_parser* tokenizer, lexer::token_view const & name, symbol_flags flags) {
        std::vector<size_t> arguments;
        if (!consume_function_parameter_list(tree, &arguments, tokenizer))
          return std::nullopt;

        lexer::token_view returnType;
        if (!tokenizer->
          parse(lexer::token_id::arrow).
          parse(lexer::token_id::identifier, &returnType).
          ok())
          return std::nullopt;

        block body;
        body.scope_name = std::string(name.name);
        if (!consume_function_body(tree, &body, tokenizer))
          return std::nullopt;

        return add_function_declaration(tree, name.name, returnType.name, std::move(arguments), tree->add(std::move(body)), flags);
      }

      std::optional<size_t> consume_fn(ast * tree, lexer::token_parser * tokenizer, symbol_flags flags) {
        lexer::token_view name;
        if (!tokenizer->
          parse(lexer::token_id::fn)
          .parse(lexer::token_id::identifier, &name)
          .parse(lexer::token_id::open_paren)
          .ok())
          return std::nullopt;

        return consume_function_declaration(tree, tokenizer, name, flags);
      }

      std::optional<size_t> consume_variable_decl(ast * tree, lexer::token_parser * tokenizer, symbol_flags flags, rules::token_rule const & terminator) {
        lexer::token_view identifier;
        lexer::token_view type_name;
        if (!tokenizer->
          parse(lexer::token_id::identifier, &identifier).
          parse(lexer::token_id::colon).
          parse(lexer::token_id::identifier, &type_name).
          ok())
          return std::nullopt;

        variable_declaration decl;
        decl.name  = identifier.name;
        decl.type_name  = type_name.name;
        decl.flags = flags;

        // Initialization statement
        if (tokenizer->current().id == lexer::token_id::assign) {
          tokenizer->next();

          decl.initializer = consume_expression(tree, tokenizer, terminator);
          if (!decl.initializer.has_value())
            return std::nullopt;
        }

        return tree->add(decl);
      }

      std::optional<size_t> consume_const(ast * tree, lexer::token_parser * tokenizer, symbol_flags flags) {
        if (!tokenizer->parse(lexer::token_id::const_).ok())
          return std::nullopt;
        return consume_variable_decl(tree, tokenizer, flags | symbol_flags::const_);
      }

      std::optional<size_t> consume_let(ast * tree, lexer::token_parser * tokenizer, symbol_flags flags) {
        if (!tokenizer->parse(lexer::token_id::let).ok())
          return std::nullopt;
        return consume_variable_decl(tree, tokenizer, flags);
      }

      std::optional<size_t> consume_init_fn(ast * tree, lexer::token_parser * tokenizer, symbol_flags flags) {
        lexer::token_view name;
        if (!tokenizer->
          parse(lexer::token_id::init)
          .parse(lexer::token_id::identifier, &name)
          .parse(lexer::token_id::open_paren)
          .ok())
          return std::nullopt;

        std::vector<size_t> arguments;
        if (!consume_function_parameter_list(tree, &arguments, tokenizer))
          return std::nullopt;

        block body;
        body.scope_name = std::string(name.name);
        if (!consume_function_body(tree, &body, tokenizer))
          return std::nullopt;

        return add_function_declaration(tree, name.name, "", std::move(arguments), tree->add(std::move(body)), flags | symbol_flags::initializer);
      }

      std::optional<size_t> consume_class(ast * tree, lexer::token_parser * tokenizer) {
        lexer::token_view identifier;
        if (!tokenizer->
          parse(lexer::token_id::class_).
          parse(lexer::token_id::identifier, &identifier).
          parse(lexer::token_id::open_brace).
          ok())
          return std::nullopt;

        class_decl cls;
        cls.identifier = identifier.name;

        while (tokenizer->current().id != lexer::token_id::close_brace) {
          switch (tokenizer->current().id) {
          case lexer::token_id::fn:     {
            auto fn = consume_fn(tree, tokenizer, symbol_flags::member);
            if (!fn.has_value())
              return std::nullopt;
            cls.methods.push_back(fn.value());
            break;
          }
          case lexer::token_id::identifier: {
            auto var = consume_variable_decl(tree, tokenizer, symbol_flags::member);
            if (!var.has_value())
              return std::nullopt;
            cls.members.push_back(var.value());
            break;
          }
          case lexer::token_id::const_: {
            auto var = consume_const(tree, tokenizer, symbol_flags::member);
            if (!var.has_value())
              return std::nullopt;
            cls.members.push_back(var.value());
            break;
          }
          case lexer::token_id::init: {
            // Parse constructor
            auto ctor = consume_init_fn(tree, tokenizer);
            if (!ctor.has_value())
              return std::nullopt;
            cls.constructors.push_back(ctor.value());
            break;
          }
          }
        }
        tokenizer->next();

        return tree->add(cls);
      }

      std::optional<size_t> consume_extern(ast * tree, lexer::token_parser * tokenizer) {
        if (!tokenizer->parse(lexer::token_id::extern_).ok())
          return std::nullopt;

        switch (tokenizer->current().id) {
        case lexer::token_id::fn:     return consume_fn(tree, tokenizer, symbol_flags::extern_);
        case lexer::token_id::const_: return consume_const(tree, tokenizer, symbol_flags::extern_);
        case lexer::token_id::identifier:   return consume_variable_decl(tree, tokenizer, symbol_flags::extern_);
        }

        return tokenizer->next();
      }
    }

    ast parse(lexer::token_parser * tokenizer) {
      ast ast;
      block topScope;
      compiler::define_builtins(&ast, &topScope);

      while (!tokenizer->eof()) {
        std::optional<size_t> nextStatement;
        switch (tokenizer->current().id) {
        case lexer::token_id::fn:       nextStatement = parser::consume_fn(&ast, tokenizer); break;
        case lexer::token_id::const_:   nextStatement = parser::consume_const(&ast, tokenizer); break;
        case lexer::token_id::let:      nextStatement = parser::consume_let(&ast, tokenizer); break;
        case lexer::token_id::class_:   nextStatement = parser::consume_class(&ast, tokenizer); break;
        case lexer::token_id::extern_:  nextStatement = parser::consume_extern(&ast, tokenizer); break;
        default:
          break;
        }

        if (nextStatement.has_value())
          topScope.statements.push_back(nextStatement.value());
        else
          break;
      }

      ast.statements.push_back(std::move(topScope));
      return ast;
    }
  }
}
