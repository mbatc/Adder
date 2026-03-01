#include "common.h"

namespace adder {
  namespace str {
    std::string_view trim_start(std::string_view const & str, std::string_view const & chars) {
      auto it = str.begin();
      for (; it < str.end(); ++it)
        if (chars.find(*it) == std::string::npos)
          break;
      return str.substr(it - str.begin());
    }

    std::string_view trim_end(std::string_view const & str, std::string_view const & chars) {
      if (str.length() == 0)
        return {};
      auto it = str.end() - 1;
      for (; it >= str.begin(); --it)
        if (chars.find(*it) == std::string::npos)
          break;
      return str.substr(0, str.end() - it - 1);
    }

    bool starts_with(std::string_view const & str, std::string_view const & needle) {
      return str.substr(0, needle.length()) == needle;
    }

    bool ends_with(std::string_view const & str, std::string_view const & needle) {
      if (str.length() < needle.length())
        return false;
      else
        return str.substr(str.length() - needle.length(), needle.length()) == needle;
    }

    std::vector<std::string_view> split(std::string_view const & str, std::vector<std::string> const & sep) {
      std::vector<std::string_view> result;

      std::string_view remaining = str;

      while (true) {
        size_t nextToken = std::string::npos;
        std::string_view found;
        for (std::string const & needle : sep) {
          size_t index = remaining.substr(0, nextToken).find_first_of(needle);
          if (index == std::string::npos)
            continue;
          nextToken = index;
          found     = remaining.substr(index, needle.size());
        }

        if (nextToken == std::string::npos)
          break;

        // Add up until `found`
        if (nextToken > 0)
          result.push_back(remaining.substr(0, nextToken));
        // Skip to the end of `found`
        remaining = remaining.substr(nextToken + found.size());
      }

      if (remaining.size() > 0)
        result.push_back(remaining);

      return result;
    }
  }
}
