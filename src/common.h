#pragma once

#include <type_traits>
#include <vector>
#include <string>

namespace adder {
  template<typename... Args>
  void unused(Args const & ...) {}

  template<typename T>
  struct enable_bitwise_ops : std::false_type {};

  template<typename T>
  inline static constexpr bool enable_bitwise_ops_v = enable_bitwise_ops<T>::value;

  namespace str {
    inline static const std::vector<std::string> whitespace = { " ", "\n", "\r", "\t", "\v" };

    std::string_view trim_start(std::string_view const & str, std::string_view const & chars = " \n\r\t\v");
    std::string_view trim_end(std::string_view const & str, std::string_view const & chars = " \n\r\t\v");
    bool starts_with(std::string_view const & str, std::string_view const & needle);
    bool ends_with(std::string_view const & str, std::string_view const & needle);
    std::vector<std::string_view> split(std::string_view const & str, std::vector<std::string> const & sep = whitespace);
  }

  template<typename... Args>
  std::string_view format(char const * fmt, Args&&... args) {
    thread_local std::vector<char> formatBuffer(512, 0);

    const int written = std::snprintf(
      formatBuffer.data(),
      formatBuffer.size(),
      fmt,
      std::forward<Args>(args)...
    );

    if (written < 0)
      return "";

    if (written < formatBuffer.size())
      return std::string_view(formatBuffer.data(), written);
    
    formatBuffer.resize(2ll << (uint64_t)log2(written));
    return format(fmt, std::forward<Args>(args)...);
  }

  template<class... Ts>
  struct overloads : Ts... { using Ts::operator()...; };
}

template<typename T, std::enable_if_t<adder::enable_bitwise_ops_v<T>>* = 0>
T operator|(T const & a, T const & b) {
  return T(std::underlying_type_t<T>(a) | std::underlying_type_t<T>(b));
}

template<typename T, std::enable_if_t<adder::enable_bitwise_ops_v<T>>* = 0>
T operator&(T const & a, T const & b) {
  return T(std::underlying_type_t<T>(a) & std::underlying_type_t<T>(b));
}

template<typename T, std::enable_if_t<adder::enable_bitwise_ops_v<T>>* = 0>
T operator^(T const & a, T const & b) {
  return T(std::underlying_type_t<T>(a) & std::underlying_type_t<T>(b));
}

template<typename T, std::enable_if_t<adder::enable_bitwise_ops_v<T>>* = 0>
T & operator|=(T & a, T const & b) {
  return a = a | b;
}

template<typename T, std::enable_if_t<adder::enable_bitwise_ops_v<T>>* = 0>
T & operator&=(T & a, T const & b) {
  return a = a & b;
}

template<typename T, std::enable_if_t<adder::enable_bitwise_ops_v<T>>* = 0>
T & operator^=(T & a, T const & b) {
  return a = a ^ b;
}
