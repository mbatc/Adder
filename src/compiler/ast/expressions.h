#pragma once

#include "../types.h"
#include <optional>
#include <map>

namespace adder {
  namespace compiler {
    struct ast;
    struct program_builder;

    namespace expr {
      struct list {
        size_t expr;
        std::optional<size_t> next;
      };

      struct type_name {
        std::string_view name;
      };

      struct type_fn {
        size_t return_type = 0; ///< Function return type.
        std::vector<size_t> argument_list; ///< List expression. Each node should contain a type
        functor_type func_type = functor_type::free;
      };

      struct type_modifier {
        bool reference  = false;
        bool const_     = false;
        size_t modified = 0;
      };

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
        std::optional<size_t> type; // If nullopt, infer the type from initializer expression
        std::string_view      name;
        symbol_flags          flags = symbol_flags::none;
        std::optional<size_t> initializer;
      };

      // struct conversion {
      //   size_t expression;
      //   std::string_view target_type_name;
      // };

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

      std::string_view get_operator_identifer(operator_type const & op);

      struct binary_operator {
        operator_type type_name;

        std::optional<size_t> left;
        std::optional<size_t> right;
      };

      struct block {
        // Name of this scope. Used to augment local public symbol names
        std::string scope_name = "";
        // Statements in this scope in sequential order.
        // TODO: If we restrict the ast datastructure a bit, perhaps this can be a body start index + count for less allocations.
        std::vector<size_t> statements;
      };

      struct byte_code {
        bool (*callback)(program_builder * program) = nullptr;
      };

      struct function_declaration {
        std::string_view identifier;
        // Linkage flags
        symbol_flags flags = symbol_flags::none;
        // Type of the function. This must be a type_fn
        std::optional<size_t> type;
        /// Arguments declarations.
        std::vector<size_t> arguments;
        /// ID of the expression that contains the function body.
        std::optional<size_t> body;
      };

      struct call_parameter {
        size_t                expression;
        std::optional<size_t> next;
      };
      
      // struct call {
      //   size_t                functor;
      //   std::optional<size_t> parameters;
      // };

      struct class_decl {
        std::string_view identifier;

        std::vector<size_t> members;
        std::vector<size_t> methods;
        std::vector<size_t> constructors;
      };

      using statement = std::variant<
        literal,
        identifier,
        list,
        type_fn,
        type_name,
        type_modifier,
        variable_declaration,
        init,
        function_return,
        binary_operator,
        block,
        byte_code,
        function_declaration,
        call_parameter,
        // call,
        class_decl
        // conversion
      >;

      constexpr size_t sz = sizeof(statement);
    }
    
    template<typename Expr, typename Visitor>
    void visit_sub_expressions(ast const& ast, size_t id, Expr const &o, Visitor const & callable) {
      unused(ast, id, o, callable);
    }

    template<typename Visitor>
    void visit_sub_expressions(ast const& ast, size_t id, expr::list const & o, Visitor const & callable) {
      unused(ast, id);

      callable(o.expr);
      if (o.next.has_value())
        callable(o.next.value());
    }

    template<typename Visitor>
    void visit_sub_expressions(ast const& ast, size_t id, expr::type_fn const &o, Visitor const & callable) {
      unused(ast, id);
      callable(o.return_type);
      for (size_t argId : o.argument_list)
        callable(argId);
    }

    template<typename Visitor>
    void visit_sub_expressions(ast const& ast, size_t id, expr::type_modifier const &o, Visitor const & callable) {
      unused(ast, id);
      callable(o.modified);
    }

    template<typename Visitor>
    void visit_sub_expressions(ast const& ast, size_t id, expr::variable_declaration const & o, Visitor const & callable) {
      unused(ast, id);
      if (o.type.has_value())
        callable(o.type.value());
      if (o.initializer.has_value())
        callable(o.initializer.value());
    }

    template<typename Visitor>
    void visit_sub_expressions(ast const& ast, size_t id, expr::init const & o, Visitor const & callable) {
      unused(ast, id);
      callable(o.target);
      callable(o.expression);
    }

    template<typename Visitor>
    void visit_sub_expressions(ast const& ast, size_t id, expr::function_return const & o, Visitor const & callable) {
      unused(ast, id);
      if (o.expression.has_value())
        callable(o.expression.value());
    }

    template<typename Visitor>
    void visit_sub_expressions(ast const& ast, size_t id, expr::binary_operator const & o, Visitor const & callable) {
      unused(ast, id);
      if (o.left.has_value())
        callable(o.left.value());
      if (o.right.has_value())
        callable(o.right.value());
    }
    
    template<typename Visitor>
    void visit_sub_expressions(ast const& ast, size_t id, expr::block const & o, Visitor const & callable) {
      unused(ast, id);
      for (size_t i = 0; i < o.statements.size(); ++i)
        callable(o.statements[i]);
    }

    template<typename Visitor>
    void visit_sub_expressions(ast const& ast, size_t id, expr::function_declaration const & o, Visitor const & callable) {
      unused(ast, id);
      for (size_t i = 0; i < o.arguments.size(); ++i)
        callable(o.arguments[i]);

      if (o.type.has_value())
        callable(o.type.value());

      if (o.body.has_value())
        callable(o.body.value());
    }

    template<typename Visitor>
    void visit_sub_expressions(ast const& ast, size_t id, expr::call_parameter const & o, Visitor const & callable) {
      unused(ast, id);
      callable(o.expression);
      if (o.next.has_value())
        callable(o.next.has_value());
    }

    template<typename Visitor>
    void visit_sub_expressions(ast const& ast, size_t id, expr::class_decl const & o, Visitor const & callable) {
      unused(ast, id);

      for (size_t i = 0; i < o.constructors.size(); ++i) {
        callable(o.constructors[i]);
      }

      for (size_t i = 0; i < o.methods.size(); ++i) {
        callable(o.methods[i]);
      }

      for (size_t i = 0; i < o.members.size(); ++i) {
        callable(o.members[i]);
      }
    }

    template<typename Visitor>
    void visit_sub_expressions(ast const& ast, size_t root, Visitor const & callable) {
      std::visit([&](auto &&o) {
        visit_sub_expressions(ast, root, o, callable);
      }, ast.statements[root]);
    }
  }
}
