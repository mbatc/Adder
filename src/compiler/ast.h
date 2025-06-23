#pragma once

#include "ast/expressions.h"

namespace adder {
  namespace compiler {
    struct ast {
      std::vector<expr::statement> statements;

      size_t get_precedence(size_t id) const;
      size_t add(expr::statement const& statement);

      template<typename T>
      T& get(size_t id) { return std::get<T>(statements[id]); }

      template<typename T>
      T const & get(size_t id) const { return std::get<T>(statements[id]); }

      template<typename T>
      bool is(size_t id) const { return std::holds_alternative<T>(statements[id]); }
    };
  }
}
