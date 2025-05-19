#include "compiler.h"
#include "common.h"
#include "lexer.h"
#include "vm.h"

#include <memory>
#include <map>
#include <variant>
#include <optional>
#include <functional>

namespace adder {
  // Compiler implementation
  namespace compiler {
    enum class symbol_type {
      function,
      variable,
    };

    enum class symbol_flags {
      none         = 0,
      // identifier         = 0,
      const_       = 1 << 0,
      extern_      = 1 << 1,
      fn_parameter = 1 << 2, ///< This variable is a function parameter
      member       = 1 << 3, ///< This symbol is a class member
      static_      = 1 << 4, ///< This symbol has static storage
      initializer  = 1 << 5, ///< This symbol has static storage
    };
  }

  template<>
  struct enable_bitwise_ops<compiler::symbol_flags> : std::true_type {};

  namespace compiler {
    struct context;
    struct type_desc {
      bool   builtin; ///< 
      size_t size;    ///< Size of the type
    };

    struct symbol_desc {
      symbol_type  symbol_type;
      std::string  identifier;
      std::string  signature;
      symbol_flags flags;
    };

    struct literal_expression {
      std::variant<
        std::string_view,
        double,
        int64_t,
        bool
      > value;
    };

    struct identifier_expression {
      std::string_view name;
    };

    struct variable_declaration {
      std::string_view type;
      std::string_view name;
      symbol_flags     flags;
      std::optional<size_t> initializer;
    };

    struct return_expression {
      std::optional<size_t> expression;
    };

    enum class operator_type { // Ordered by precedence
      unknown = -1,
      assign,
      equal,
      not_equal,
      greater_equal,
      greater,
      less_equal,
      less,
      add,
      minus,
      divide,
      multiply,
      bang,
      call,
      dot,
      count,
    };

    struct binary_operator {
      operator_type type;

      std::optional<size_t> left;
      std::optional<size_t> right;
    };

    struct block {
      // Index of the parent block scope.
      // Allows us to walk up the tree.
      std::optional<size_t> parent_scope;
      // Name of this scope. Used to augment local public symbol names
      std::string scope_name = "";
      // Types that have been parsed
      std::map<std::string, type_desc> types;
      // Symbols in the current scope
      std::vector<symbol_desc> symbols;
      // Function bodies
      std::map<std::string, size_t> functions;
      // Statements in this scope in sequential order.
      std::vector<size_t> statements;
    };

    struct function_declaration {
      // Return type for this scope.
      std::string_view           identifier;
      std::string                signature;
      symbol_flags               flags = symbol_flags::none;
      std::optional<size_t>      body;
      std::optional<std::string> return_type;
    };

    struct call_parameter {
      size_t                expression;
      std::optional<size_t> next;
    };

    struct call_expression {
      size_t                functor;
      std::optional<size_t> parameters;
    };

    struct class_decl {
      std::string_view identifier;

      std::vector<size_t> members;
      std::vector<size_t> methods;
      std::vector<size_t> constructors;
    };

    using statement = std::variant<
      literal_expression,
      identifier_expression,
      variable_declaration,
      return_expression,
      binary_operator,
      block,
      function_declaration,
      call_parameter,
      call_expression,
      class_decl
    >;

    struct context {
      std::vector<statement> statements;

      size_t get_precedence(size_t id) const
      {
        if (std::holds_alternative<binary_operator>(statements[id]))
          return (size_t)std::get<binary_operator>(statements[id]).type;
        else
          return std::numeric_limits<size_t>::max();
      }

      size_t add(statement const & statement) {
        statements.push_back(statement);
        return statements.size() - 1;
      }

      template<typename T>
      T& get(size_t id) { return std::get<T>(statements[id]); }

      template<typename T>
      T const & get(size_t id) const { return std::get<T>(statements[id]); }

      template<typename T>
      bool is(size_t id) const { return std::holds_alternative<T>(statements[id]); }
    };

    namespace rules {
      struct token_rule {
        token_rule(std::function<bool(lexer::token_view const &)> const & func)
          : pred(func)
        {}

        token_rule(lexer::token_id id) : pred([=](lexer::token_view const & token) { return token.id == id; }) {}
        token_rule(lexer::token_class cls) : pred([=](lexer::token_view const & token) { return token.cls == cls; }) {}

        bool operator()(lexer::token_view const & token) const {
          return pred(token);
        }

        std::function<bool(lexer::token_view const &)>  pred;
      };


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

    inline static operator_type get_operator_type(lexer::token_id token) {
      switch (token) {
      case lexer::token_id::assign:        return operator_type::assign;
      case lexer::token_id::equal:         return operator_type::equal;
      case lexer::token_id::not_equal:     return operator_type::not_equal;
      case lexer::token_id::less_equal:    return operator_type::less_equal;
      case lexer::token_id::greater_equal: return operator_type::greater_equal;
      case lexer::token_id::less:          return operator_type::less;
      case lexer::token_id::greater:       return operator_type::greater;
      case lexer::token_id::bang:          return operator_type::bang;
      case lexer::token_id::multiply:      return operator_type::multiply;
      case lexer::token_id::divide:        return operator_type::divide;
      case lexer::token_id::add:           return operator_type::add;
      case lexer::token_id::minus:         return operator_type::minus;
      case lexer::token_id::dot:           return operator_type::dot;
      }
      return operator_type::unknown;
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

    symbol_desc variable_symbol(std::string_view const & identifier, std::string_view const & type, symbol_flags flags = symbol_flags::none) {
      return {
        symbol_type::variable,
        std::string(identifier),
        std::string(type),
        flags
      };
    }

    symbol_desc function_symbol(std::string_view const & identifier, std::string_view const & signature, symbol_flags flags = symbol_flags::none) {
      return {
        symbol_type::function,
        std::string(identifier),
        std::string(signature),
        flags
      };
    }

    symbol_desc * find_symbol(context * state, block * scope, std::string_view const & identifier) {
      for (auto & desc : scope->symbols) {
        if (desc.identifier == identifier) {
          return &desc;
        }
      }

      if (!scope->parent_scope.has_value())
        return find_symbol(state, &std::get<block>(state->statements[scope->parent_scope.value()]), identifier);
      return nullptr;
    }

    std::optional<size_t> consume_literal(context * state, lexer::token_parser * tokenizer);
    std::optional<size_t> consume_expression(context * state, lexer::token_parser * tokenizer, rules::token_rule const & terminator = lexer::token_id::semi_colon);
    std::optional<size_t> consume_const(context * state, lexer::token_parser * tokenizer, symbol_flags flags = symbol_flags::none);
    std::optional<size_t> consume_let(context * state, lexer::token_parser * tokenizer, symbol_flags flags = symbol_flags::none);
    std::optional<size_t> consume_fn(context * state, lexer::token_parser * tokenizer, symbol_flags flags = symbol_flags::none);
    std::optional<size_t> consume_class(context * state, lexer::token_parser * tokenizer);
    std::optional<size_t> consume_extern(context * state, lexer::token_parser * tokenizer);
    std::optional<size_t> consume_variable_decl(context* state, lexer::token_parser* tokenizer, symbol_flags flags, rules::token_rule const& terminator = lexer::token_id::semi_colon);

    std::optional<size_t> consume_return(context * state, lexer::token_parser * tokenizer) {
      if (!tokenizer->parse(lexer::token_id::return_).ok()) {
        return std::nullopt;
      }

      std::optional<size_t> expression = consume_expression(state, tokenizer);
      state->add(return_expression{ expression });
      return true;
    }

    std::optional<size_t> consume_block(context * state, lexer::token_parser * tokenizer) {
      if (!tokenizer->parse(lexer::token_id::open_brace).ok())
        return std::nullopt;

      block scope;
      scope.scope_name   = std::to_string(state->statements.size() + 1);
      while (tokenizer->current().id != lexer::token_id::close_brace)
      {
        auto expression = consume_expression(state, tokenizer);
        if (!expression.has_value())
          break;

        scope.statements.push_back(expression.value());
      }

      if (tokenizer->eof())
        return std::nullopt;

      return state->add(std::move(scope));
    }

    std::optional<size_t> consume_parameter(context * state, lexer::token_parser * tokenizer) {
      return consume_variable_decl(state, tokenizer, symbol_flags::fn_parameter | symbol_flags::const_, rules::or(lexer::token_id::comma, lexer::token_id::close_paren));
    }

    bool takes_precedence(context * state, size_t statement, size_t over) {
      return state->get_precedence(statement) > state->get_precedence(over);
    }

    // Assumes newStatement takes precedence over parentId
    bool insert_statement(context * state, size_t parentId, size_t newStatementId) {
      if (!state->is<binary_operator>(parentId)) {
        return false;
      }

      auto& parent = state->get<binary_operator>(parentId);
      if (parent.right.has_value() && takes_precedence(state, newStatementId, parent.right.value())) {
        return insert_statement(state, parent.right.value(), newStatementId);
      }
      else {
        auto oldRight = parent.right;
        parent.right = newStatementId;

        if (oldRight.has_value()) {
          if (state->is<binary_operator>(newStatementId)) {
            state->get<binary_operator>(newStatementId).left = oldRight;
          }
          else {
            // oldRight was lost... something bad may happen
            return false;
          }
        }
      }

      return true;
    }

    bool insert_statement(context * state, std::optional<size_t> *pRoot, size_t newStatementId) {
      if (!pRoot->has_value()) {
        *pRoot = newStatementId;
        return true;
      }

      if (takes_precedence(state, pRoot->value(), newStatementId)) {
        auto &op = state->get<binary_operator>(newStatementId);

        // Make next statement top of tree
        op.left = *pRoot;
        *pRoot = std::move(newStatementId);
        return true;
      }
      else {
        return insert_statement(state, pRoot->value(), newStatementId);
      }
    }

    // bool insert_statement(context * state, size_t parentId, size_t scopeId, lexer::token_view token) {
    // 
    //   state->statements.push_back(create_statement(token));
    // 
    //   return insert_statement(state, scopeId, parentId, state->statements.size() - 1);
    // }

    std::optional<size_t> consume_call(context * state, lexer::token_parser * tokenizer) {
      if (!tokenizer->parse(lexer::token_id::open_paren).ok())
        return std::nullopt;

      binary_operator call;
      call.type  = operator_type::call;
      std::optional<size_t> lastParameter;
      while (tokenizer->current().id != lexer::token_id::close_paren) {
        std::optional<size_t> expression = consume_expression(state, tokenizer, rules::or(lexer::token_id::comma, lexer::token_id::close_paren));
        if (!expression.has_value())
          return std::nullopt;

        call_parameter param;
        param.expression = expression.value();
        size_t paramId = state->add(param);

        if (lastParameter.has_value()) {
          state->get<call_parameter>(lastParameter.value()).next = paramId;
        }

        if (!call.right.has_value()) {
          call.right = paramId;
        }

        lastParameter = paramId;

        if (tokenizer->previous().id == lexer::token_id::close_paren)
          break;
      }

      return state->add(call);
    }

    bool is_callable(context * state, size_t expressionId) {
      return !state->is<binary_operator>(expressionId) || state->get<binary_operator>(expressionId).type == operator_type::call;
    }

    std::optional<size_t> consume_literal(context* state, lexer::token_parser* tokenizer) {
      lexer::token_view value;
      if (!tokenizer->parse(lexer::token_class::literal_, &value).ok())
        return std::nullopt;

      switch (value.id) {
      case lexer::token_id::true_:          return state->add(literal_expression{ true });
      case lexer::token_id::false_:         return state->add(literal_expression{ false });
      case lexer::token_id::integer:        return state->add(literal_expression{ std::atoll(std::string(value.name).c_str()) });
      case lexer::token_id::decimal:        return state->add(literal_expression{ std::atof(std::string(value.name).c_str()) });
      case lexer::token_id::string_literal: return state->add(literal_expression{ value.name });
      default: break;
      }

      return std::nullopt;
    }

    std::optional<size_t> consume_expression(context * state, lexer::token_parser * tokenizer, rules::token_rule const & terminator) {
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
          if (ret.has_value() && is_callable(state, ret.value()))
            subExpression = consume_call(state, tokenizer);
          else
            subExpression = consume_expression(state, tokenizer, lexer::token_id::close_paren);
          break;
        case lexer::token_id::open_brace:
          subExpression = consume_block(state, tokenizer);
          break;
        case lexer::token_id::identifier:
          subExpression = state->add(identifier_expression{ token.name });
          tokenizer->next();
          break;
        case lexer::token_id::return_:
          return consume_return(state, tokenizer);
        case lexer::token_id::let:
          return consume_let(state, tokenizer);
        case lexer::token_id::const_:
          return consume_const(state, tokenizer);
        default: {
          if (token.cls == lexer::token_class::literal_) {
            subExpression = consume_literal(state, tokenizer);
          }
          else {
            if (!tokenizer->parse(lexer::token_class::operator_, &token).ok())
              return std::nullopt;

            auto operatorType = get_operator_type(token.id);
            binary_operator op;
            op.type = operatorType;
            subExpression = state->add(op);
          }
          break;
        }
        }

        if (!subExpression.has_value())
          return std::nullopt;

        insert_statement(state, &ret, subExpression.value());
      }

      return ret;
    }

    std::optional<size_t> consume_statement(context* state, lexer::token_parser* tokenizer) {
      switch (tokenizer->current().id) {
      case lexer::token_id::fn:         return consume_fn(state, tokenizer);
      case lexer::token_id::class_:     return consume_class(state, tokenizer);
      case lexer::token_id::extern_:    return consume_extern(state, tokenizer);
      case lexer::token_id::open_brace: return consume_block(state, tokenizer);
        // case lexer::token_id::if_:        return consume_if(state, tokenizer);
      default:                          return consume_expression(state, tokenizer);
      }
    }

    std::optional<size_t> consume_function_declaration(context* state, lexer::token_parser* tokenizer, lexer::token_view const & name, symbol_flags flags) {
      function_declaration function;
      block body;
      body.scope_name = "$" + std::string(name.name);

      while (tokenizer->current().id != lexer::token_id::close_paren) {
        auto paramId = consume_parameter(state, tokenizer);
        if (!paramId.has_value())
          return std::nullopt;

        body.statements.push_back(paramId.value());

        if (tokenizer->current().id == lexer::token_id::close_paren)
          break;
        if (tokenizer->current().id != lexer::token_id::comma)
          return std::nullopt;

        tokenizer->next();
      }
      tokenizer->next();

      lexer::token_view returnType;
      if (!tokenizer->
        parse(lexer::token_id::arrow).
        parse(lexer::token_id::identifier, &returnType).
        ok())
        return std::nullopt;

      std::string signature = "(";
      {
        function.return_type = returnType.name;
        size_t numParams = 0;
        for (auto & paramId : body.statements) {
          auto &decl = state->get<variable_declaration>(paramId);

          if ((decl.flags & symbol_flags::fn_parameter) == symbol_flags::fn_parameter) {
            if (numParams != 0)
              signature += ",";
            signature += decl.type;
            ++numParams;
          }
        }
        signature += ")=>" + std::string(returnType.name);
      }

      lexer::token_view token;
      if (!tokenizer->
        parse(rules::or(
          lexer::token_id::semi_colon,
          lexer::token_id::open_brace), &token)
        .ok())
        return std::nullopt;

      if (token.id == lexer::token_id::open_brace) {
        // Parse function body.
        while (tokenizer->current().id != lexer::token_id::close_brace) {
          std::optional<size_t> expression = consume_expression(state, tokenizer);
          if (!expression.has_value())
            return std::nullopt;

          body.statements.push_back(expression.value());
        }

        tokenizer->next();
      }

      std::string fullName(name.name);
      fullName += signature;
      function.identifier = name.name;
      function.signature  = fullName;
      function.flags      = flags;
      function.body = state->add(std::move(body));

      return state->add(function);
    }

    std::optional<size_t> consume_fn(context * state, lexer::token_parser * tokenizer, symbol_flags flags) {
      lexer::token_view name;
      if (!tokenizer->
        parse(lexer::token_id::fn)
        .parse(lexer::token_id::identifier, &name)
        .parse(lexer::token_id::open_paren)
        .ok())
        return std::nullopt;

      return consume_function_declaration(state, tokenizer, name, flags);
    }

    std::optional<size_t> consume_variable_decl(context * state, lexer::token_parser * tokenizer, symbol_flags flags, rules::token_rule const & terminator) {
      lexer::token_view identifier;
      lexer::token_view type;
      if (!tokenizer->
        parse(lexer::token_id::identifier, &identifier).
        parse(lexer::token_id::colon).
        parse(lexer::token_id::identifier, &type).
        ok())
        return std::nullopt;

      variable_declaration decl;
      decl.name  = identifier.name;
      decl.type  = type.name;
      decl.flags = flags;

      // Initialization statement
      if (tokenizer->current().id == lexer::token_id::assign) {
        tokenizer->next();

        decl.initializer = consume_expression(state, tokenizer, terminator);
        if (!decl.initializer.has_value())
          return std::nullopt;
      }

      return state->add(decl);
    }

    std::optional<size_t> consume_const(context * state, lexer::token_parser * tokenizer, symbol_flags flags) {
      if (!tokenizer->parse(lexer::token_id::const_).ok())
        return std::nullopt;
      return consume_variable_decl(state, tokenizer, flags | symbol_flags::const_);
    }

    std::optional<size_t> consume_let(context * state, lexer::token_parser * tokenizer, symbol_flags flags) {
      if (!tokenizer->parse(lexer::token_id::let).ok())
        return std::nullopt;
      return consume_variable_decl(state, tokenizer, flags);
    }

    std::optional<size_t> consume_init_fn(context * state, lexer::token_parser * tokenizer, symbol_flags flags = symbol_flags::none) {
      lexer::token_view name;
      if (!tokenizer->
        parse(lexer::token_id::init)
        .parse(lexer::token_id::identifier, &name)
        .parse(lexer::token_id::open_paren)
        .ok())
        return std::nullopt;

      return consume_function_declaration(state, tokenizer, name, flags);
    }

    std::optional<size_t> consume_class(context * state, lexer::token_parser * tokenizer) {
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
          auto fn = consume_fn(state, tokenizer, symbol_flags::member);
          if (!fn.has_value())
            return std::nullopt;
          cls.methods.push_back(fn.value());
          break;
        }
        case lexer::token_id::identifier: {
          auto var = consume_variable_decl(state, tokenizer, symbol_flags::member);
          if (!var.has_value())
            return std::nullopt;
          cls.members.push_back(var.value());
          break;
        }
        case lexer::token_id::const_: {
          auto var = consume_const(state, tokenizer, symbol_flags::member);
          if (!var.has_value())
            return std::nullopt;
          cls.members.push_back(var.value());
          break;
        }
        case lexer::token_id::init: {
          // Parse constructor
          auto ctor = consume_init_fn(state, tokenizer);
          if (!ctor.has_value())
            return std::nullopt;
          cls.constructors.push_back(ctor.value());
          break;
        }
        }
      }

      return state->add(cls);
    }

    std::optional<size_t> consume_extern(context * state, lexer::token_parser * tokenizer) {
      if (!tokenizer->parse(lexer::token_id::extern_).ok())
        return std::nullopt;

      switch (tokenizer->current().id) {
      case lexer::token_id::fn:     return consume_fn(state, tokenizer, symbol_flags::extern_);
      case lexer::token_id::const_: return consume_const(state, tokenizer, symbol_flags::extern_);
      case lexer::token_id::identifier:   return consume_variable_decl(state, tokenizer, symbol_flags::extern_);
      }

      return tokenizer->next();
    }

    context parse(lexer::token_parser * tokenizer) {
      context ast;
      block topScope;

      while (!tokenizer->eof()) {
        std::optional<size_t> nextStatement;
        switch (tokenizer->current().id) {
        case lexer::token_id::fn:       nextStatement = consume_fn(&ast, tokenizer); break;
        case lexer::token_id::const_:   nextStatement = consume_const(&ast, tokenizer); break;
        case lexer::token_id::let:      nextStatement = consume_let(&ast, tokenizer); break;
        case lexer::token_id::class_:   nextStatement = consume_class(&ast, tokenizer); break;
        case lexer::token_id::extern_:  nextStatement = consume_extern(&ast, tokenizer); break;
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

    program generate_code(context const & context) {
      context;
      return {};
    }

    program compile(std::string const & source) {
      lexer::token_parser tokenizer(source);
      context ast = parse(&tokenizer);

      if (!tokenizer.ok()) {
        for (auto & error : tokenizer.errors()) {
          printf("Error: %s\n", error.c_str());
        }
      }

      return generate_code(ast);
    }
  }
}
