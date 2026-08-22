#pragma once

#include <string_view>
#include <string>
#include <vector>
#include <optional>

namespace adder {
  namespace vm {
    struct machine;
  }

  template<bool Const>
  struct program_view_impl;
  struct program;
  using program_view       = program_view_impl<false>;
  using const_program_view = program_view_impl<true>;

  std::optional<program> compile(vm::machine * vm, std::string const & module_name, std::string const & source);
}
