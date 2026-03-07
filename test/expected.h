#pragma once

#include <vector>
#include <string>
#include <string_view>
#include <optional>

namespace adder {
  namespace test {
    class expected {
    public:
      struct symbol {
        std::string name;
        std::string value;
        bool exists = false;
      };

      static std::optional<expected> parse(std::string_view const & config);

      std::optional<std::string> entry = "()=>void:main";
      std::vector<symbol> symbols;
    };
  }
}
