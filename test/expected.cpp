#include "expected.h"
#include "common.h"

namespace adder {
  namespace test {
    static std::optional<std::string_view> parse_value(std::string_view const& line) {
      auto keyEnd = line.find_first_of(':');
      if (keyEnd == std::string::npos)
        return std::nullopt;

      auto value = line.substr(keyEnd + 1);
      return str::trim_end(str::trim_start(value));
    }
    static std::optional<std::string_view> parse_value(std::vector<std::string_view> const& lines, size_t* pIndex) {
      auto line = lines[*pIndex];
      ++(*pIndex);
      return parse_value(line);
    }

    static std::optional<expected::symbol> parse_symbol(std::vector<std::string_view> const & lines, size_t *pIndex) {
      const size_t rootIndent = lines[*pIndex].find_first_not_of(' ');
      ++(*pIndex);
      expected::symbol ret;
      for (; *pIndex < lines.size(); ++(*pIndex)) {
        auto ln = lines[*pIndex];
        const size_t entryIndent = ln.find_first_not_of(' ');
        if (entryIndent == std::string::npos)
          continue;
        if (entryIndent <= rootIndent)
          break;

        auto value = parse_value(ln);
        if (!value.has_value())
          return std::nullopt;

        auto trimmed = str::trim_start(ln);
        if (str::starts_with(trimmed, "name:")) {
          ret.name = value.value();
        }
        else if (str::starts_with(trimmed, "exists:")) {
          ret.exists = value.value() == "true";
        }
        else if (str::starts_with(trimmed, "value:")) {
          ret.value = value.value();
        }
        else {
          return std::nullopt;
        }
      }
      return ret;
    }

    std::optional<expected> expected::parse(std::string_view const & config)
    {
      expected ret;
      std::vector<std::string_view> lines = str::split(config, { "\n" });

      for (size_t i = 0; i < lines.size();) {
        auto line = lines[i];
        auto trimmed = str::trim_end(line);
        if (trimmed.length() == 0) {
          ++i;
          continue;
        }
        if (trimmed == "symbol:") {
          auto symbol = parse_symbol(lines, &i);
          if (!symbol.has_value()) {
            return std::nullopt;
          }
          ret.symbols.push_back(symbol.value());
        }
        else if (str::starts_with(trimmed, "entry:")) {
          auto entry = parse_value(lines, &i);
          if (!entry.has_value()) {
            return std::nullopt;
          }
          if (entry == "none")
            ret.entry.reset();
          else
            ret.entry = entry.value();
        }
        else {
          return std::nullopt; // Bad file
        }
      }
      return ret;
    }
  }
}
