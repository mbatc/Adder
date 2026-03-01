#pragma once

#include <string>
#include <vector>

namespace adder {
  namespace compiler {
    namespace lexer {
      enum class token_class {
        unknown = -1,
        identifier,
        literal_,
        operator_,
        keyword,
        grammar,
        count,
      };

      enum class token_id {
        unknown = -1,

        identifier, // Identifier
        string_literal,
        true_,
        false_,
        integer,
        decimal,

        // Keywords
        fn,
        init,
        class_,
        this_,
        extern_,
        as,
        const_,
        ref,
        let,

        if_,
        else_,
        elseif,
        for_,

        return_,

        // Grammar
        colon,
        arrow,
        semi_colon,
        open_paren,
        close_paren,
        open_brace,
        close_brace,
        open_bracket,
        close_bracket,
        comma,
        new_line,
        line_comment,
        open_block_comment,
        close_block_comment,
        quote,
        eof,

        // Operators
        assign,
        equal,
        not_equal,
        less_equal,
        greater_equal,
        less,
        greater,
        bang,
        multiply,
        divide,
        add,
        minus,
        dot,

        count
      };
      inline static constexpr size_t token_id_count = (size_t)token_id::count;
      
      std::string_view token_id_to_string(token_id const & id);

      struct token_desc {
        std::string name;
        token_id id;
        token_class cls;
      };

      extern const std::vector<token_desc> tokens;

      struct token_view {
        token_id id;
        token_class cls;
        std::string_view name;

        size_t line   = 1;
        size_t column = 1;
      };

      class token_parser {
      public:
        token_parser() = default;
        token_parser(std::string _source);

        token_parser& parse(token_id const & id, token_view * storage = nullptr);
        token_parser& parse(token_class const & cls, token_view * storage = nullptr);

        template<typename Rule>
        token_parser& parse(Rule const & rule, token_view * storage = nullptr);

        /// true if no errors have occured.
        bool ok() const;

        /// Parse until the next token is found.
        /// Currently parses out comments.
        bool next();

        token_view const & current() const;
        token_view const & previous() const;
        bool eof() const;

        std::vector<std::string> const& errors() const;

      private:
        bool _next();

        void raise(std::string const & error);
        bool parseLineComment();
        bool parseBlockComment();

        std::string      m_source;
        std::string_view m_remaining;

        token_view m_previous;
        token_view m_current;

        std::vector<std::string> m_errors;
      };

      template<typename Rule>
      token_parser& token_parser::parse(Rule const & rule, token_view * storage) {
        if (!rule(current())) {
          raise("Unexpected token '" + std::string(current().name) + "' (line: " + std::to_string(current().line) + ", col: " + std::to_string(current().column) + ")");
          return *this;
        }

        if (storage != nullptr)
          *storage = current();

        if (!next()) {
          raise("Unexpected eof reached");
        }

        return *this;
      }
    }
  }
}
