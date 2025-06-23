#pragma once

#include "../common.h"
#include "../vm.h"

#include <variant>

namespace adder {
  // Compiler implementation
  namespace compiler {
    enum class symbol_flags {
      none         = 0,
      // identifier         = 0,
      const_       = 1 << 0,
      extern_      = 1 << 1,
      fn_parameter = 1 << 2, ///< This variable is a function parameter
      member       = 1 << 3, ///< This symbol is a class member
      static_      = 1 << 4, ///< This symbol has static storage
      initializer  = 1 << 5, ///< This a class initializer method
    };
  }

  template<>
  struct enable_bitwise_ops<compiler::symbol_flags> : std::true_type {};

  namespace compiler {
    enum class type_primitive {
      unknown = -1,
      _void,
      int8,
      int16,
      int32,
      int64,
      uint8,
      uint16,
      uint32,
      uint64,
      float32,
      float64,
      bool_,
    };

    struct class_desc {
      size_t size = 0; ///< Size of the type
      std::vector<std::string> members;
      std::vector<std::string> methods;
      std::vector<std::string> constructors;
    };

    struct function_desc {
      size_t size = sizeof(vm::address_t);
      size_t return_type;
      std::vector<size_t> arguments;
    };

    struct type {
      std::string_view identifier;
      std::variant<type_primitive, class_desc, function_desc> desc;
    };

    size_t type_size(type_primitive const & desc);
    size_t type_size(class_desc const & desc);
    size_t type_size(function_desc const & desc);
    size_t type_size(type const & type);
  }
}
