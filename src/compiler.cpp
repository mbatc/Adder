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
      const_       = 1 << 0,
      extern_      = 1 << 1,
      fn_parameter = 1 << 2, ///< This variable is a function parameter
    };
  }

  template<>
  struct enable_bitwise_ops<compiler::symbol_flags> : std::true_type {};

  namespace compiler {
    struct type_desc {
      bool   builtin; ///< 
      size_t size;    ///< Size of the type
    };

    struct statement {
      statement() = default;
      statement(lexer::token_view token) : token(token) {};

      lexer::token_view token;

      std::unique_ptr<statement> left  = nullptr;
      std::unique_ptr<statement> right = nullptr;
    };

    struct symbol_desc {
      symbol_type  symbol_type;
      std::string  identifier;
      std::string  signature;
      symbol_flags flags;
    };

    struct context {
      // Pointer to the parent scope.
      // Allows us to walk up the tree.
      context * parent_scope;
      // Name of this scope. Used to augment local public symbol names
      std::string scope_name = "";
      // Types that have been parsed
      std::map<std::string, type_desc> types;
      // Symbols in the current scope
      std::vector<symbol_desc> symbols;
      // Function bodies
      std::map<std::string, std::unique_ptr<context>> functions;
      // Statements in this scope in sequential order.
      std::vector<
        std::variant<
        std::unique_ptr<statement>,
        std::unique_ptr<context>
        >> statements;

      // Statement currently being parsed
      std::unique_ptr<statement> currentStatement;

      // Return type for this scope.
      std::optional<std::string> return_type;
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

    symbol_desc * find_symbol(context * state, std::string_view const & identifier) {
      for (auto & desc : state->symbols) {
        if (desc.identifier == identifier) {
          return &desc;
        }
      }
      return nullptr;
    }

    bool consume_code(context * state, lexer::token_parser * tokenizer);
    bool consume_fn(context * state, lexer::token_parser * tokenizer, symbol_flags flags = symbol_flags::none);
    bool consume_const(context * state, lexer::token_parser * tokenizer);
    bool consume_let(context * state, lexer::token_parser * tokenizer);
    bool consume_class(context * state, lexer::token_parser * tokenizer);
    bool consume_extern(context * state, lexer::token_parser * tokenizer);

    bool consume_block(context * state, lexer::token_parser * tokenizer) {
      if (!tokenizer->parse(lexer::token_id::open_brace).ok())
        return false;

      context block;
      block.parent_scope = state;
      block.scope_name   = state->scope_name + "$" + std::to_string(state->statements.size() + 1);

      while (tokenizer->current().id != lexer::token_id::close_brace)
        consume_code(&block, tokenizer);

      if (tokenizer->eof())
        return false;

      state->statements.push_back(std::make_unique<context>(std::move(block)));

      return true;
    }

    bool consume_parameter(context * state, lexer::token_parser * tokenizer) {
      lexer::token_view identifier = tokenizer->current();
      lexer::token_view type = tokenizer->current();
      if (!tokenizer->
        parse(lexer::token_id::none, &identifier)
        .parse(lexer::token_id::colon)
        .parse(lexer::token_id::none, &type)
        .ok())
        return false;

      state->symbols.push_back(variable_symbol(identifier.name, type.name, symbol_flags::fn_parameter));

      return true;
    }

    bool takes_precedence(lexer::token_id op, lexer::token_id over) {
      static const std::vector<lexer::token_id> precedence = {
        lexer::token_id::assign,
        lexer::token_id::equal,
        lexer::token_id::not_equal,
        lexer::token_id::greater_equal,
        lexer::token_id::greater,
        lexer::token_id::less_equal,
        lexer::token_id::less,
        lexer::token_id::add,
        lexer::token_id::minus,
        lexer::token_id::divide,
        lexer::token_id::multiply,
        lexer::token_id::bang,
        lexer::token_id::call,
        lexer::token_id::dot,
        lexer::token_id::none, // Evaluate identifiers and literals first
      };

      return std::find(precedence.begin(), precedence.end(), op) > std::find(precedence.begin(), precedence.end(), over);
    }

    // Assumes newStatement takes precedence over 'parent'
    bool insert_statement(std::unique_ptr<statement> const & parent, std::unique_ptr<statement> &&newStatement) {
      if (parent->right != nullptr && takes_precedence(newStatement->token.id, parent->right->token.id)) {
        return insert_statement(parent->right, std::move(newStatement));
      }
      else {
        auto oldRight = std::move(parent->right);
        parent->right = std::move(newStatement);

        // newStatement is always to the right of any statements in `parent`.
        // insert oldRight to the left of newStatement.
        parent->right->left = std::move(oldRight);
      }

      return true;
    }

    bool insert_statement(context * state, std::unique_ptr<statement> &&newStatement) {
      if (state->currentStatement == nullptr) {
        state->currentStatement = std::move(newStatement);
        return true;
      }

      if (takes_precedence(state->currentStatement->token.id, newStatement->token.id)) {
        // Make next statement top of tree
        newStatement->left = std::move(state->currentStatement);
        state->currentStatement = std::move(newStatement);
        return true;
      }
      else {
        return insert_statement(state->currentStatement, std::move(newStatement));
      }
    }

    bool insert_statement(context * state, lexer::token_view token) {
      return insert_statement(state, std::make_unique<statement>(token));
    }

    std::unique_ptr<statement> consume_call(context * state, lexer::token_parser * tokenizer) {
      // TODO: Parse argument list into subnodes of the 'call' statement
      unused(state, tokenizer);

      lexer::token_view callToken;
      callToken.id = lexer::token_id::call;
      callToken.cls = lexer::token_class::operator_;

      return std::make_unique<statement>(callToken);
    }

    bool consume_statement(context * state, lexer::token_parser * tokenizer) {
      lexer::token_view token;
      if (!tokenizer->parse(
        rules:: or (
          rules:: or (
            lexer::token_id::open_brace,
            lexer::token_class::operator_
            ),
          rules::or(
            lexer::token_id::none,
            lexer::token_id::semi_colon
            )
          ), &token)
        .ok())
        return false;

      if (token.id == lexer::token_id::semi_colon)
      {
        if (state->currentStatement != nullptr)
          state->statements.push_back(std::move(state->currentStatement));
        return true;
      }

      if (token.id == lexer::token_id::open_brace) {
        insert_statement(state, consume_call(state, tokenizer));
      }
      else {
        insert_statement(state, token);
      }

      return true;
    }

    bool consume_code(context * state, lexer::token_parser * tokenizer) {
      switch (tokenizer->current().id) {
      case lexer::token_id::fn:         return consume_fn(state, tokenizer);
      case lexer::token_id::const_:     return consume_const(state, tokenizer);
      case lexer::token_id::let:        return consume_let(state, tokenizer);
      case lexer::token_id::class_:     return consume_class(state, tokenizer);
      case lexer::token_id::extern_:    return consume_extern(state, tokenizer);
      case lexer::token_id::open_brace: return consume_block(state, tokenizer);
        // case lexer::token_id::if_:        return consume_if(state, tokenizer);
      case lexer::token_id::new_line:   return tokenizer->next();
      default:
        return consume_statement(state, tokenizer);
      }
    }

    bool consume_fn(context * state, lexer::token_parser * tokenizer, symbol_flags flags) {
      lexer::token_view name;
      if (!tokenizer->
        parse(lexer::token_id::fn)
        .parse(lexer::token_id::none, &name)
        .parse(lexer::token_id::open_paren)
        .ok())
        return false;

      context functionScope;
      functionScope.parent_scope = state;
      functionScope.types = state->types;
      functionScope.scope_name = state->scope_name + "$" + std::string(name.name);

      while (tokenizer->current().id != lexer::token_id::close_paren) {
        consume_parameter(&functionScope, tokenizer);

        if (tokenizer->current().id == lexer::token_id::close_paren)
          break;
        if (tokenizer->current().id != lexer::token_id::comma)
          return false;
        tokenizer->next();
      }
      tokenizer->next();

      lexer::token_view returnType;
      if (!tokenizer->
        parse(lexer::token_id::arrow).
        parse(lexer::token_id::none, &returnType).
        ok())
        return false;

      functionScope.return_type = returnType.name;
      std::string signature = "(";
      size_t numParams = 0;
      for (auto & symbol : functionScope.symbols) {
        if ((symbol.flags & symbol_flags::fn_parameter) == symbol_flags::fn_parameter) {
          if (numParams != 0)
            signature += ",";
          signature += symbol.signature;
          ++numParams;
        }
      }
      signature += ")=>" + std::string(returnType.name);

      lexer::token_view token;
      if (!tokenizer->
        parse(rules::or(
          lexer::token_id::semi_colon,
          lexer::token_id::open_brace), &token)
        .ok())
        return false;

      if (token.id == lexer::token_id::semi_colon)
        return true;

      // Parse function body.
      while (tokenizer->current().id != lexer::token_id::close_brace)
        consume_code(&functionScope, tokenizer);

      std::string fullName(name.name);
      fullName += signature;

      state->symbols.push_back(function_symbol(name.name, signature, flags));
      state->functions[fullName] = std::make_unique<context>(std::move(functionScope));

      return !tokenizer->eof();
    }

    bool consume_const(context * state, lexer::token_parser * tokenizer) {
      lexer::token_view identifier;
      lexer::token_view type;
      if (!tokenizer->
        parse(lexer::token_id::const_).
        parse(lexer::token_id::none, &identifier).
        parse(lexer::token_id::colon).
        parse(lexer::token_id::none, &type).
        ok())
        return false;

      state->symbols.push_back(variable_symbol(identifier.name, type.name, symbol_flags::const_));
      state->currentStatement = std::make_unique<statement>(identifier);

      return consume_statement(state, tokenizer);
    }

    bool consume_let(context * state, lexer::token_parser * tokenizer) {
      lexer::token_view identifier;
      lexer::token_view type;
      if (!tokenizer->
        parse(lexer::token_id::let).
        parse(lexer::token_id::none, &identifier).
        parse(lexer::token_id::colon).
        parse(lexer::token_id::none, &type).
        ok())
        return false;

      state->symbols.push_back(variable_symbol(identifier.name, type.name));
      state->currentStatement = std::make_unique<statement>(identifier);

      return consume_statement(state, tokenizer);
    }

    bool consume_class(context * state, lexer::token_parser * tokenizer) {
      unused(state, tokenizer);
      return tokenizer->next();
    }

    bool consume_extern(context * state, lexer::token_parser * tokenizer) {
      if (!tokenizer->parse(lexer::token_id::extern_).ok())
        return false;

      if (tokenizer->current().id == lexer::token_id::fn) {
        return consume_fn(state, tokenizer, symbol_flags::extern_);
      }
      unused(state, tokenizer);
      return tokenizer->next();
    }

    // Consume top level
    bool consume_top_level_statement(context * state, lexer::token_parser * tokenizer) {
      switch (tokenizer->current().id) {
      case lexer::token_id::fn:       return consume_fn(state, tokenizer);
      case lexer::token_id::const_:   return consume_const(state, tokenizer);
      case lexer::token_id::let:      return consume_let(state, tokenizer);
      case lexer::token_id::class_:   return consume_class(state, tokenizer);
      case lexer::token_id::extern_:  return consume_extern(state, tokenizer);
      case lexer::token_id::new_line: return tokenizer->next();
      }

      return false;
    }

    program compile(std::string const & source) {
      lexer::token_parser tokenizer(source);

      context root_scope;
      while (consume_top_level_statement(&root_scope, &tokenizer));

      return {};
    }
  }
}
