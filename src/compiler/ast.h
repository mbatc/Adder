#pragma once

#include "ast/expressions.h"

namespace adder {
  namespace compiler {
    struct ast {
      std::vector<expr::statement> statements;

      size_t get_precedence(size_t id) const;
      size_t add(expr::statement const& statement);

      template<typename T>
      std::optional<size_t> id_of(T const * statement) const {
        int64_t estimate = (uint8_t*)statement - (uint8_t*)statements.data();
        if (estimate < 0 || estimate >= (int64_t)statements.size())
          return std::nullopt;

        for (int64_t i = estimate; i < statements.size(); ++i)
          if (std::holds_alternative<T>(statements[i]) && &std::get<T>(statements[i]) == statement)
            return i;

        return std::nullopt;
      }

      template<typename T>
      T& get(size_t id) { return std::get<T>(statements[id]); }

      template<typename T>
      T const & get(size_t id) const { return std::get<T>(statements[id]); }

      template<typename T>
      bool is(size_t id) const { return std::holds_alternative<T>(statements[id]); }
    };
  }
}
