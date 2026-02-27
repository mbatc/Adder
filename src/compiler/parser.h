#pragma once

#include "ast/expressions.h"
#include "lexer.h"
#include <functional>

namespace adder {
  namespace compiler {
    struct ast;

    ast parse(lexer::token_parser* tokenizer);

    namespace parser {
      namespace rules {
        struct token_rule {
          token_rule(std::function<bool(lexer::token_view const&)> const& func);
          token_rule(lexer::token_id id);
          token_rule(lexer::token_class cls);

          bool operator()(lexer::token_view const& token) const;

          std::function<bool(lexer::token_view const &)>  pred;
        };

        token_rule or (token_rule const& a, token_rule const& b);
        token_rule xor (token_rule const& a, token_rule const& b);
        token_rule and (token_rule const& a, token_rule const& b);
      }

      expr::operator_type get_operator_type(lexer::token_id token);
      std::optional<size_t> consume_type_expression(ast* tree, lexer::token_parser* tokenizer);
      std::optional<size_t> consume_literal(ast * tree, lexer::token_parser * tokenizer);
      std::optional<size_t> consume_expression(ast * tree, lexer::token_parser * tokenizer, rules::token_rule const & terminator = lexer::token_id::semi_colon);
      std::optional<size_t> consume_const(ast * tree, lexer::token_parser * tokenizer, symbol_flags flags = symbol_flags::none);
      std::optional<size_t> consume_let(ast * tree, lexer::token_parser * tokenizer, symbol_flags flags = symbol_flags::none);
      std::optional<size_t> consume_fn(ast * tree, lexer::token_parser * tokenizer, functor_type funcType, symbol_flags flags = symbol_flags::none);
      std::optional<size_t> consume_class(ast * tree, lexer::token_parser * tokenizer);
      std::optional<size_t> consume_extern(ast * tree, lexer::token_parser * tokenizer);
      std::optional<size_t> consume_variable_decl(ast* tree, lexer::token_parser* tokenizer, symbol_flags flags, rules::token_rule const& terminator = lexer::token_id::semi_colon);
      std::optional<size_t> consume_return(ast* tree, lexer::token_parser* tokenizer);
      std::optional<size_t> consume_block(ast* tree, lexer::token_parser* tokenizer);
      std::optional<size_t> consume_parameter(ast* tree, lexer::token_parser* tokenizer);
      bool takes_precedence(ast* tree, size_t statement, size_t over);
      // Assumes newStatement takes precedence over parentId
      bool insert_statement(ast* tree, size_t parentId, size_t newStatementId);
      bool insert_statement(ast* tree, std::optional<size_t>* pRoot, size_t newStatementId);
      std::optional<size_t> consume_call(ast* tree, lexer::token_parser* tokenizer);
      bool is_callable(ast* tree, size_t expressionId);
      std::optional<size_t> consume_init_expression(ast* tree, lexer::token_parser* tokenizer);
      std::optional<size_t> consume_statement(ast* tree, lexer::token_parser* tokenizer);
      bool consume_function_parameter_list(ast* tree, std::vector<size_t> * arguments, lexer::token_parser* tokenizer);
      bool consume_function_body(ast * tree, std::string_view const & scopeName, std::optional<size_t> * body, lexer::token_parser * tokenizer);
      // std::optional<size_t> add_function_declaration(ast * tree, std::string_view identifier, size_t returnType, std::vector<size_t> &&arguments, size_t bodyId, symbol_flags flags, functor_type funcType);
      // std::optional<size_t> consume_function_declaration(ast* tree, lexer::token_parser* tokenizer, lexer::token_view const& name, symbol_flags flags);
      std::optional<size_t> consume_variable_decl(ast* tree, lexer::token_parser* tokenizer, symbol_flags flags, rules::token_rule const& terminator);
      std::optional<size_t> consume_init_fn(ast* tree, lexer::token_parser* tokenizer, symbol_flags flags = symbol_flags::none);
      std::optional<size_t> consume_class(ast* tree, lexer::token_parser* tokenizer);
      std::optional<size_t> consume_extern(ast* tree, lexer::token_parser* tokenizer);
    }
  }
}
