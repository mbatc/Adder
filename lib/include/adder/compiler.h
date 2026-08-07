#pragma once

#include <string_view>
#include <string>
#include <vector>
#include <optional>

namespace adder {
  template<bool Const>
  struct program_view_impl;
  struct program;
  using program_view       = program_view_impl<false>;
  using const_program_view = program_view_impl<true>;

  std::optional<program> compile(std::string const & source);
}
