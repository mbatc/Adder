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
    enum class symbol_flags {
      none         = 0,
      // identifier         = 0,
      const_       = 1 << 0,
      extern_      = 1 << 1,
      fn_parameter = 1 << 2, ///< This variable is a function parameter
      member       = 1 << 3, ///< This symbol is a class member
      static_      = 1 << 4, ///< This symbol has static storage
      initializer  = 1 << 5, ///< This a class initializer method
    };
  }

  template<>
  struct enable_bitwise_ops<compiler::symbol_flags> : std::true_type {};

  namespace compiler {
    struct context;

    enum class type_primitive {
      unknown = -1,
      int8,
      int16,
      int32,
      int64,
      uint8,
      uint16,
      uint32,
      uint64,
      float32,
      float64,
      bool_,
    };

    struct class_desc {
      size_t size = 0; ///< Size of the type
      std::vector<std::string> members;
      std::vector<std::string> methods;
      std::vector<std::string> constructors;
    };

    struct function_desc {
      size_t size = sizeof(vm::address_t);
      std::vector<std::string> arguments;
      std::string return_type;
    };

    using type_desc = std::variant<type_primitive, class_desc>;

    namespace impl {
      size_t type_size(type_primitive const& desc) {
        switch (desc) {
        case type_primitive::int8: return sizeof(int8_t);
        case type_primitive::int16: return sizeof(int16_t);
        case type_primitive::int32: return sizeof(int32_t);
        case type_primitive::int64: return sizeof(int64_t);
        case type_primitive::uint8: return sizeof(uint8_t);
        case type_primitive::uint16: return sizeof(uint16_t);
        case type_primitive::uint32: return sizeof(uint32_t);
        case type_primitive::uint64: return sizeof(uint64_t);
        case type_primitive::float32: return sizeof(float);
        case type_primitive::float64: return sizeof(double);
        case type_primitive::bool_: return sizeof(bool);
        }
        return 0;
      }

      size_t type_size(class_desc const& desc) {
        return desc.size;
      }

      size_t type_size(function_desc const& desc) {
        return desc.size;
      }
    }

    size_t type_size(type_desc const& desc) {
      return std::visit([](auto const & o) {
        return impl::type_size(o);
      }, desc);
    }

    using type_desc = std::variant<type_primitive, class_desc>;

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
      std::string_view type_name;
      std::string_view name;
      symbol_flags     flags;
      std::optional<size_t> initializer;
    };

    struct type_conversion {
      size_t expression;
      std::string_view target_type_name;
      // type_desc * target_type_desc;
    };

    struct init_expression {
      size_t target;
      size_t expression;
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
      operator_type type_name;

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
      // std::vector<symbol_desc> symbols;
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
      std::optional<std::string> return_type_name;
      // type_desc *                return_type_desc = nullptr;
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
      init_expression,
      return_expression,
      binary_operator,
      block,
      function_declaration,
      call_parameter,
      call_expression,
      class_decl,
      type_conversion
    >;

    struct context {
      std::vector<statement> statements;

      size_t get_precedence(size_t id) const
      {
        if (std::holds_alternative<binary_operator>(statements[id]))
          return (size_t)std::get<binary_operator>(statements[id]).type_name;
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

    // symbol_desc variable_symbol(std::string_view const & identifier, std::string_view const & type_name, symbol_flags flags = symbol_flags::none) {
    //   return {
    //     symbol_type::variable,
    //     std::string(identifier),
    //     std::string(type_name),
    //     flags
    //   };
    // }
    // 
    // symbol_desc function_symbol(std::string_view const & identifier, std::string_view const & signature, symbol_flags flags = symbol_flags::none) {
    //   return {
    //     symbol_type::function,
    //     std::string(identifier),
    //     std::string(signature),
    //     flags
    //   };
    // }
    // 
    // symbol_desc * find_symbol(context * state, block * scope, std::string_view const & identifier) {
    //   for (auto & desc : scope->symbols) {
    //     if (desc.identifier == identifier) {
    //       return &desc;
    //     }
    //   }
    // 
    //   if (!scope->parent_scope.has_value())
    //     return find_symbol(state, &std::get<block>(state->statements[scope->parent_scope.value()]), identifier);
    //   return nullptr;
    // }

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
      call.type_name  = operator_type::call;
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
      return !state->is<binary_operator>(expressionId) || state->get<binary_operator>(expressionId).type_name == operator_type::call;
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

    std::optional<size_t> consume_init_expression(context* state, lexer::token_parser* tokenizer) {
      lexer::token_view value;
      if (!tokenizer->parse(lexer::token_id::init).ok())
        return std::nullopt;

      std::optional<size_t> target     = consume_expression(state, tokenizer, lexer::token_id::as);
      std::optional<size_t> expression = consume_expression(state, tokenizer);

      if (!target.has_value() || !expression.has_value())
        return std::nullopt;

      init_expression initializer;
      initializer.target = target.value();
      initializer.expression = expression.value();
      return state->add(initializer);
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
          if (get_operator_type(previous.id) == operator_type::unknown) {
            subExpression = consume_call(state, tokenizer);
          } else {
            tokenizer->next();
            subExpression = consume_expression(state, tokenizer, lexer::token_id::close_paren);
          }
          break;
        case lexer::token_id::open_brace:
          subExpression = consume_block(state, tokenizer);
          break;
        case lexer::token_id::identifier:
          subExpression = state->add(identifier_expression{ token.name });
          tokenizer->next();
          break;
        case lexer::token_id::this_:
          subExpression = state->add(identifier_expression{ token.name });
          tokenizer->next();
          break;
        case lexer::token_id::return_:
          return consume_return(state, tokenizer);
        case lexer::token_id::let:
          return consume_let(state, tokenizer);
        case lexer::token_id::const_:
          return consume_const(state, tokenizer);
        case lexer::token_id::init:
          return consume_init_expression(state, tokenizer);
        default: {
          if (token.cls == lexer::token_class::literal_) {
            subExpression = consume_literal(state, tokenizer);
          }
          else {
            if (!tokenizer->parse(lexer::token_class::operator_, &token).ok())
              return std::nullopt;

            auto operatorType = get_operator_type(token.id);
            binary_operator op;
            op.type_name = operatorType;
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

    bool consume_function_parameter_list(context * state, block * body, lexer::token_parser * tokenizer) {
      while (tokenizer->current().id != lexer::token_id::close_paren) {
        auto paramId = consume_parameter(state, tokenizer);
        if (!paramId.has_value())
          return false;

        body->statements.push_back(paramId.value());

        if (tokenizer->current().id == lexer::token_id::close_paren)
          break;
        if (tokenizer->current().id != lexer::token_id::comma)
          return false;

        tokenizer->next();
      }
      tokenizer->next();
      return true;
    }

    bool consume_function_body(context* state, block* body, lexer::token_parser* tokenizer) {
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
          std::optional<size_t> expression = consume_expression(state, tokenizer);
          if (!expression.has_value())
            return false;

          body->statements.push_back(expression.value());
        }

        tokenizer->next();
      }
      return true;
    }

    std::optional<size_t> add_function_declaration(context* state, std::string_view identifier, std::string_view returnType, size_t bodyId, symbol_flags flags) {
      auto& body = state->get<block>(bodyId);

      std::string signature = "(";
      size_t numParams = 0;
      for (auto & paramId : body.statements) {
        if (!state->is<variable_declaration>(paramId))
          break;
        auto &decl = state->get<variable_declaration>(paramId);
        if ((decl.flags & symbol_flags::fn_parameter) != symbol_flags::fn_parameter)
          break;
        if (numParams != 0)
          signature += ",";
        signature += decl.type_name;
        ++numParams;
      }
      signature += ")=>" + std::string(returnType);

      function_declaration function;
      function.identifier  = identifier;
      function.signature   = std::string(identifier) + signature;
      function.return_type_name = returnType;
      function.flags       = flags;
      function.body        = bodyId;

      return state->add(function);
    }

    std::optional<size_t> consume_function_declaration(context* state, lexer::token_parser* tokenizer, lexer::token_view const & name, symbol_flags flags) {
      block body;
      body.scope_name = std::string(name.name);

      if (!consume_function_parameter_list(state, &body, tokenizer))
        return std::nullopt;

      lexer::token_view returnType;
      if (!tokenizer->
        parse(lexer::token_id::arrow).
        parse(lexer::token_id::identifier, &returnType).
        ok())
        return std::nullopt;

      if (!consume_function_body(state, &body, tokenizer))
        return std::nullopt;

      return add_function_declaration(state, name.name, returnType.name, state->add(std::move(body)), flags);
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

      block body;
      body.scope_name = std::string(name.name);

      if (!consume_function_parameter_list(state, &body, tokenizer))
        return std::nullopt;

      if (!consume_function_body(state, &body, tokenizer))
        return std::nullopt;

      return add_function_declaration(state, name.name, "", state->add(std::move(body)), flags | symbol_flags::initializer);
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
      tokenizer->next();

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

    /// States we need to track while generating code for a program
    struct program_builder {
      struct symbol_desc {
        std::string_view  type_name;
        std::string_view  identifier;
        symbol_flags flags = symbol_flags::none;

        ///< Scope that this symbol was declared in.
        int64_t scope_index = 0;

        ///< Offset from the scopes stack pointer to this variable.
        std::optional<uint64_t> offset = 0;
        std::optional<size_t>   size   = 0;
      };

      struct scope {
        std::vector<symbol_desc> symbols;

        uint64_t stackSize = 0;
      };

      // struct relocation {
      //   uint64_t offset; ///< Where the resolve symbol is written to.
      //   uint64_t symbol; ///< The index of the symbol in the program.
      // };

      struct expression_result {
        std::optional<vm::register_value> constant;
        std::optional<uint64_t> stack_address; ///< Address of the value
        std::string_view type_name; ///< Type of the value
      };

      struct {
        std::vector<vm::register_index> free;
        vm::register_index next = 0;

        vm::register_index pin() {
          vm::register_index idx = 0;
          if (free.size() == 0) {
            idx = next++;
          }
          else {
            idx = free.front();
            free.erase(free.begin());
          }
          return idx;
        }

        void release(vm::register_index idx) {
          free.push_back(idx);
        }
      } registers;

      std::vector<vm::instruction>    code;
      // std::vector<relocation>         relocations;
      std::vector<scope>              scopes;
      std::vector<expression_result>  results;

      std::map<std::string_view, type_desc, std::less<>> types = {
        { "int8",    { type_primitive::int8 } },
        { "int16",   { type_primitive::int16 } },
        { "int32",   { type_primitive::int32 } },
        { "int64",   { type_primitive::int64 } },
        { "uint8",   { type_primitive::int8 } },
        { "uint16",  { type_primitive::uint16 } },
        { "uint32",  { type_primitive::uint32 } },
        { "uint64",  { type_primitive::uint64 } },
        { "float32", { type_primitive::float32  }},
        { "float64", { type_primitive::float64 } },
        { "bool",    { type_primitive::bool_ } }
      };

      type_desc const * get_type(std::string_view const& name) const {
        auto it = types.find(name);
        if (it == types.end())
          return nullptr;
        return &it->second;
      }

      symbol_desc const * find_symbol(std::string_view const& identifier) const {
        for (size_t i = scopes.size() - 1; i >= 0; --i) {
          for (auto& symbol : scopes[i].symbols) {
            if (symbol.identifier == identifier) {
              return &symbol;
            }
          }
        }

        return nullptr;
      }

      bool add_type(std::string_view const& name, type_desc const & desc) {
        types[name] = desc;
      }

      bool push_scope() {
        scopes.emplace_back();
      }

      bool pop_scope() {
        scopes.pop_back();
      }

      bool push_symbol(std::string_view const& identifier, symbol_desc const& desc) {
        symbol_desc newSymbol = desc;
        scope &block = scopes.back();
        if (newSymbol.size.has_value())
        {
          newSymbol.offset = block.stackSize;
          block.stackSize += newSymbol.size.value();
        }
        newSymbol.identifier  = identifier;
        newSymbol.scope_index = scopes.size() - 1;
        block.symbols.push_back(newSymbol);
        return true;
      }

      bool pop_symbol(size_t count = 1) {
        scope &block = scopes.back();
        while (count-- > 0)
        {
          symbol_desc back = block.symbols.back();
          block.symbols.pop_back();

          if (back.offset.has_value())
            block.stackSize = back.offset.value();
        }

        return true;
      }

      bool push_variable(std::string_view const& identifier, std::string_view const & type_name, symbol_flags const & flags) {
        symbol_desc symbol;
        symbol.flags       = flags;
        symbol.type_name   = type_name;
        symbol.identifier  = identifier;
        symbol.size        = type_size(types[type_name]);
        return push_symbol(identifier, symbol);
      }

      void push_expression_result(expression_result result) {
        results.push_back(result);
      }

      vm::register_index pin_constant(vm::register_value value) {
        vm::register_index idx = registers.pin();
        vm::instruction op;
        op.code = vm::op_code::set;
        op.set.val = value;
        op.set.dst = idx;
        add_instruction(op);
        return idx;
      }

      vm::register_index pin_address(uint64_t address, size_t size) {
        vm::register_index idx = registers.pin();
        vm::instruction op;
        op.code = vm::op_code::load_addr;
        op.load_addr.addr = address;
        op.load_addr.size = size;
        op.load_addr.dst  = idx;
        op.set.dst = idx;
        add_instruction(op);
        return idx;
      }

      vm::register_index pin_result(expression_result value, size_t size) {
        if (value.constant.has_value()) {
          return pin_constant(value.constant.value());
        }
        else if (value.stack_address.has_value()) {
          return pin_address(value.stack_address.value(), size);
        }
        else {
          // src = program->load_relocated_address(value.symbol_name);
        }
        return registers.pin();
      }

      void release_register(vm::register_index reg) {
      }

      expression_result pop_expression_result() {
        expression_result ret = results.back();
        results.pop_back();
        return ret;
      }

      uint64_t evaluate_stack_address(symbol_desc const & symbol) {
        return scopes[symbol.scope_index].stackSize - symbol.offset.value();
      }

      void add_instruction(vm::instruction inst) {
        code.push_back(inst);
      }

      /// Convert the program to a binary
      program binary() const {
        return {};
      }
    };

    bool generate_code(context const& ast, program_builder* program, size_t statementId);

    bool generate_literal_code(context const& ast, program_builder* program, bool value) {
      unused(ast);

      program_builder::expression_result result;
      result.constant.emplace(0);
      std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_name = "bool";
      program->push_expression_result(result);
      return true;
    }

    bool generate_literal_code(context const& ast, program_builder* program, int64_t value) {
      program_builder::expression_result result;
      result.constant.emplace(0);
      std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_name = "int64";
      program->push_expression_result(result);
      return true;
    }

    bool generate_literal_code(context const& ast, program_builder* program, double value) {
      program_builder::expression_result result;
      result.constant.emplace(0);
      std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_name = "float64";
      program->push_expression_result(result);
      return true;
    }

    bool generate_literal_code(context const& ast, program_builder* program, std::string_view const & value) {
      program_builder::expression_result result;
      // result.constant.emplace(0);
      // result.symbol = program->push_data_symbol(value.data(), value.length());
      // std::memcpy(&result.constant.value(), &value, sizeof(value));
      result.type_name = "char[]";
      program->push_expression_result(result);
      return true;
    }

    bool generate_code(context const & ast, program_builder * program, literal_expression const & statement) {
      return std::visit(
        [=](auto value) {
          return generate_literal_code(ast, program, value);
        },
        statement.value);
    }

    bool generate_code(context const & ast, program_builder * program, identifier_expression const & statement) {
      program_builder::symbol_desc const * symbol = program->find_symbol(statement.name);
      if (symbol == nullptr) {
        return false;
      }

      program_builder::expression_result result;
      if (symbol->offset.has_value()) {
        result.stack_address = program->evaluate_stack_address(*symbol);
      }
      else {
        // Need a relocation to evaluate the address once we know where the symbol is actually stored.
      }
      program->push_expression_result(result);
      return true;
    }

    bool generate_code(context const & ast, program_builder * program, variable_declaration const & statement) {
      type_desc const * typeInfo = program->get_type(std::string(statement.type_name));

      vm::instruction clr;
      clr.set.dst = 0;
      clr.set.val = 0;

      vm::instruction psh;
      psh.push.size = type_size(*typeInfo);
      psh.push.src  = 0;

      program->add_instruction(clr);
      program->add_instruction(psh);
      program->push_variable(statement.name, statement.type_name, statement.flags);
      return true;
    }

    bool generate_init_code(context const & ast, program_builder * program, program_builder::expression_result target, type_primitive targetType, program_builder::expression_result value, type_primitive valueType) {
      switch (targetType) {
      case type_primitive::int64: {
        vm::register_index dst = program->pin_result(target, type_size(targetType));
        vm::register_index src = program->pin_result(value, type_size(valueType));
        vm::instruction str;
        str.code = vm::op_code::store;
        str.store.dst_addr = dst;
        str.store.src = src;
        program->add_instruction(str);
        program->release_register(dst);
        program->release_register(src);
      } break;
      }
      return true;
    }

    bool generate_init_code(context const & ast, program_builder * program, program_builder::expression_result target, class_desc targetType, program_builder::expression_result value, class_desc valueType) {

      return true;
    }

    bool generate_init_code(context const & ast, program_builder * program, program_builder::expression_result target, type_primitive targetType, program_builder::expression_result value, class_desc valueType) {

      return true;
    }

    bool generate_init_code(context const & ast, program_builder * program, program_builder::expression_result target, class_desc targetType, program_builder::expression_result value, type_primitive valueType) {

      return true;
    }

    bool generate_code(context const & ast, program_builder * program, init_expression const& statement) {
      generate_code(ast, program, statement.expression);
      auto value = program->pop_expression_result();

      generate_code(ast, program, statement.target);
      auto target = program->pop_expression_result();

      type_desc const * targetType = program->get_type(target.type_name);
      type_desc const * valueType = program->get_type(value.type_name);

      if (targetType == nullptr || valueType == nullptr) {
        return false;
      }

      return std::visit([&](const auto & a) {
        return std::visit([&](const auto & b) {
          return generate_init_code(ast, program, target, a, value, b);
          }, *valueType);
      }, *targetType);
    }

    bool generate_code(context const & ast, program_builder * program, return_expression const& statement) {

      return true;
    }

    bool generate_code(context const & ast, program_builder * program, binary_operator const& statement) {

      return true;
    }

    bool generate_code(context const & ast, program_builder * program, function_declaration const& statement) {

      return true;
    }

    bool generate_code(context const & ast, program_builder * program, call_parameter const& statement) {

      return true;
    }

    bool generate_code(context const & ast, program_builder * program, call_expression const& statement) {

      return true;
    }

    bool generate_code(context const & ast, program_builder * program, type_conversion const & statement) {

      return true;
    }

    bool generate_code(context const & ast, program_builder * program, class_decl const& statement) {

      return true;
    }

    bool generate_code(context const & ast, program_builder * program, block const & scope) {
      for (size_t statementId : scope.statements)
        if (!generate_code(ast, program, statementId))
          return false;
      return true;
    }

    bool generate_code(context const & ast, program_builder * program, size_t statementId) {
      bool result = false;
      std::visit([=, &result](auto const & statement) {
        result = generate_code(ast, program, statement);
      }, ast.statements[statementId]);
      return result;
    }

    program generate_code(context const & ast) {
      program_builder ret;
      block const & top = ast.get<block>(ast.statements.size() - 1);
      for (size_t statementId : top.statements) {
        generate_code(ast, &ret, statementId);
      }

      return ret.binary();
    }

    program compile(std::string const & source) {
      lexer::token_parser tokenizer(source);
      context ast = parse(&tokenizer);

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
}
