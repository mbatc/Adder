#include "ast.h"
#include "ast/builtins.h"

namespace adder {
  namespace compiler {
    ast::ast() {
      define_builtins(this);
    }

    size_t ast::get_precedence(size_t id) const
    {
      if (std::holds_alternative<expr::binary_operator>(statements[id]))
        return (size_t)std::get<expr::binary_operator>(statements[id]).type_name;
      else
        return std::numeric_limits<size_t>::max();
    }

    size_t ast::add(expr::statement const & statement) {
      statements.push_back(statement);
      return statements.size() - 1;
    }
  }
}

