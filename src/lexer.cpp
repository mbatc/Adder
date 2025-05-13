#include "lexer.h"
#include "common.h"

#include <optional>
#include <algorithm>
#include <string>
#include <vector>

namespace adder {
  namespace compiler {
    namespace lexer {
      const std::vector<token_desc> tokens = {
        { "fn",     token_id::fn,                  token_class::keyword },
        { "init",   token_id::init,                token_class::keyword },
        { "this",   token_id::this_,               token_class::keyword },
        { "class",  token_id::class_,               token_class::keyword },
        { "extern", token_id::extern_,             token_class::keyword },
        { "as",     token_id::as,                  token_class::keyword },
        { "const",  token_id::const_,              token_class::keyword },
        { "let",    token_id::let,                 token_class::keyword },
        { ":",      token_id::colon,               token_class::grammar},
        { "=>",     token_id::arrow,               token_class::grammar},
        { ";",      token_id::semi_colon,          token_class::grammar},
        { "(",      token_id::open_paren,          token_class::grammar},
        { ")",      token_id::close_paren,         token_class::grammar},
        { "{",      token_id::open_brace,          token_class::grammar},
        { "}",      token_id::close_brace,         token_class::grammar},
        { ",",      token_id::comma,               token_class::grammar},
        { "\n",     token_id::new_line,            token_class::grammar},
        { "//",     token_id::line_comment,        token_class::grammar},
        { "/*",     token_id::open_block_comment,  token_class::grammar},
        { "*/",     token_id::close_block_comment, token_class::grammar},
        { "\"",     token_id::quote,               token_class::grammar},
        // { "", token_id::eof,           token_class::grammar}, // eof token is not parsed. it is appended to the input
        { "=",      token_id::assign,              token_class::operator_ },
        { "==",     token_id::equal,               token_class::operator_ },
        { "!=",     token_id::not_equal,           token_class::operator_ },
        { "<=",     token_id::less_equal,          token_class::operator_ },
        { ">=",     token_id::greater_equal,       token_class::operator_ },
        { "<",      token_id::less,                token_class::operator_ },
        { ">",      token_id::greater,             token_class::operator_ },
        { "!",      token_id::bang,                token_class::operator_ },
        { "*",      token_id::multiply,            token_class::operator_ },
        { "/",      token_id::divide,              token_class::operator_ },
        { "+",      token_id::add,                 token_class::operator_ },
        { "-",      token_id::minus,               token_class::operator_ },
        { ".",      token_id::dot,                 token_class::operator_ },
      };

      // Sorted by token length. When tokenizing, we search for the longest token first.
      // This is to avoid conflicting with longer tokens including shorted tokens (e.g. '>' and '>=')
      static const std::vector<token_desc> sorted_tokens = []() {
        auto sorted = tokens;
        std::sort(sorted.begin(), sorted.end(), [](token_desc const & a, token_desc const & b) {
          return a.name.length() > b.name.length();
          });
        return sorted;
        }();

      static const std::string token_terminators = []() {
        std::string chars;
        chars = " \r\n\t\v";
        for (auto & token : tokens)
          if (token.cls == token_class::operator_ || token.cls == token_class::grammar)
            for (char c : token.name)
              if (chars.find(c) == std::string::npos)
                chars += c;
        return chars;
        }();

      token_parser::token_parser(std::string _source)
        : m_source(std::move(_source)) {
        m_remaining = m_source;
        m_remaining = str::trim_start(m_remaining, " \r\t\v");

        next();
      }

      token_parser& token_parser::parse(token_id const & id, token_view * storage) {
        return parse([=](token_view const & token) -> bool { return token.id == id; }, storage);
      }

      token_parser& token_parser::parse(token_class const & cls, token_view * storage) {
        return parse([=](token_view const & token) -> bool { return token.cls == cls; }, storage);
      }

      /// true if no errors have occured.
      bool token_parser::ok() const {
        return m_errors.size() == 0;
      }

      /// Parse until the next token is found.
      /// Currently parses out comments.
      bool token_parser::next() {
        if (m_remaining.length() == 0) {
          if (m_current.id == token_id::eof)
            return false;
          m_current.id = token_id::eof;
          m_current.name = "";
          m_current.cls = token_class::grammar;
        }

        std::optional<token_desc> candidate;

        for (auto const & token : sorted_tokens) {
          if (str::starts_with(m_remaining, token.name)) {
            candidate = token;
            break;
          }
        }

        size_t end = candidate.has_value() ? candidate->name.length() : 0;
        if (!candidate.has_value() || (candidate->cls != token_class::grammar && candidate->cls != token_class::operator_))
          end = m_remaining.find_first_of(token_terminators, end);

        std::string_view nextToken = m_remaining.substr(0, end);
        if (!candidate.has_value() || end != candidate->name.length()) {
          m_current.id  = token_id::none;
          m_current.cls = token_class::unknown;
        }
        else {
          m_current.id   = candidate->id;
          m_current.cls  = candidate->cls;
        }
        m_current.name = m_remaining.substr(0, end);
        m_remaining    = str::trim_start(m_remaining.substr(end), " \r\t\v"); // Skip whitespace (except \n as we tokenize new lines)

        if (current().id == token_id::open_block_comment)
          return parseBlockComment();
        if (current().id == token_id::line_comment)
          return parseLineComment();
        return true;
      }

      token_view const & token_parser::current() const {
        return m_current;
      }

      bool token_parser::eof() const {
        return current().id == token_id::eof;
      }

      void token_parser::raise(std::string const & error) {
        m_errors.push_back(error);
      }

      bool token_parser::parseLineComment() {
        while (next() && current().id != token_id::new_line);
        if (current().id != token_id::eof)
          next();
        return true;
      }

      bool token_parser::parseBlockComment() {
        while (next())
        {
          if (current().id == token_id::close_block_comment)
            return true;

          if (current().id == token_id::open_block_comment)
            if (!parseBlockComment())
              return false;
        }

        return false;
      }
    }
  }
}
