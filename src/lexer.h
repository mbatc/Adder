#pragma once

#include <string>
#include <vector>

namespace adder {
  namespace compiler {
    namespace lexer {
      enum class token_class {
        unknown = -1,
        identifier,
        literal,
        operator_,
        keyword,
        grammar,
        count,
      };

      enum class token_id {
        none, // Identifier

        // Keywords
        fn,
        init,
        class_,
        this_,
        extern_,
        as,
        const_,
        let,

        if_,
        else_,
        elseif,

        // Grammar
        colon,
        arrow,
        semi_colon,
        open_paren,
        close_paren,
        open_brace,
        close_brace,
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
        call,

        count
      };
      inline static constexpr size_t token_id_count = (size_t)token_id::count;

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
        bool eof() const;

      private:
        void raise(std::string const & error);
        bool parseLineComment();
        bool parseBlockComment();

        std::string      m_source;
        std::string_view m_remaining;
        token_view       m_current;

        std::vector<std::string> m_errors;
      };

      template<typename Rule>
      token_parser& token_parser::parse(Rule const & rule, token_view * storage) {
        if (!rule(current())) {
          raise("Unexpected token " + std::string(current().name));
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
