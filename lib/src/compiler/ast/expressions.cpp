#include "compiler/ast/expressions.h"
#include "compiler/lexer.h"

namespace adder {
  namespace compiler {
    namespace expr {
      std::string_view get_operator_identifer(operator_type const& op) {
        using namespace lexer;
        token_id tok = token_id::unknown;
        switch (op) {
        case operator_type::assign:        tok = token_id::assign; break;
        case operator_type::equal:         tok = token_id::equal; break;
        case operator_type::not_equal:     tok = token_id::not_equal; break;
        case operator_type::less_equal:    tok = token_id::less_equal; break;
        case operator_type::greater_equal: tok = token_id::greater_equal; break;
        case operator_type::less:          tok = token_id::less; break;
        case operator_type::greater:       tok = token_id::greater; break;
        case operator_type::bang:          tok = token_id::bang; break;
        case operator_type::multiply:      tok = token_id::multiply; break;
        case operator_type::divide:        tok = token_id::divide; break;
        case operator_type::add:           tok = token_id::add; break;
        case operator_type::minus:         tok = token_id::minus; break;
        case operator_type::dot:           tok = token_id::dot; break;
        }
        return token_id_to_string(tok);
      }

    }
  }
}