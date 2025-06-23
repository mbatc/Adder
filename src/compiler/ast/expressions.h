#pragma once

#include "../types.h"
#include <optional>
#include <map>

namespace adder {
  namespace compiler {
    struct ast;
    struct program_builder;

    namespace expr {
      struct literal {
        std::variant<
          std::string_view,
          double,
          int64_t,
          bool
        > value;
      };

      struct identifier {
        std::string_view name;
      };

      struct variable_declaration {
        std::string_view type_name;
        std::string_view name;
        symbol_flags     flags;
        std::optional<size_t> initializer;
      };

      struct conversion {
        size_t expression;
        std::string_view target_type_name;
        // type_desc * target_type_desc;
      };

      struct init {
        size_t target;
        size_t expression;
      };

      struct function_return {
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
        std::map<std::string, type> types;
        // Symbols in the current scope
        // std::vector<symbol_desc> symbols;
        // Function bodies
        std::map<std::string, size_t> functions;
        // Statements in this scope in sequential order.
        std::vector<size_t> statements;
      };

      struct byte_code {
        bool (*callback)(program_builder * program) = nullptr;
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

      struct call {
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
        literal,
        identifier,
        variable_declaration,
        init,
        function_return,
        binary_operator,
        block,
        byte_code,
        function_declaration,
        call_parameter,
        call,
        class_decl,
        conversion
      >;
    }
  }
}
