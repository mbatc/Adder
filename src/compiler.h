#pragma once

#include <string_view>
#include <string>
#include <vector>

namespace adder {
  template<bool Const>
  struct program_view_impl;
  struct program;
  using program_view       = program_view_impl<false>;
  using const_program_view = program_view_impl<true>;

  program compile(std::string const & source);
}
