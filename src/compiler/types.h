#pragma once

#include "../common.h"
#include "../vm.h"

#include <variant>
#include <optional>

namespace adder {
  // Compiler implementation
  namespace compiler {
    enum class symbol_flags {
      none         = 0,
      const_       = 1 << 0, ///< Symbol can not be mutated.
      extern_      = 1 << 1, ///< External to the program. Symbol is located by the host.
      fn_parameter = 1 << 2, ///< This variable is a function parameter
      static_      = 1 << 3, ///< This symbol has static storage
      inline_      = 1 << 5, ///< This symbol can be inlined where possible
    };
  }

  template<>
  struct enable_bitwise_ops<compiler::symbol_flags> : std::true_type {};

  namespace compiler {
    enum class type_primitive {
      unknown = -1,
      void_,
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

    /// Type of function declaraition
    enum class functor_type {
      free,
      member,
      initializer,
      operator_,
    };

    struct type_class {
      size_t size = 0; ///< Size of the class in bytes
      std::vector<std::string> members;
      std::vector<std::string> methods;
      std::vector<std::string> constructors;
    };

    struct type_function {
      /// Size of a variable of this type in bytes.
      size_t size = sizeof(vm::address_t);
      /// Index of the return type definition
      size_t return_type = 0;
      /// Indices of argument type definitions
      std::vector<size_t> arguments;
      /// Type of method.
      functor_type func_type = functor_type::free;
    };
    
    struct type_function_decl {
      /// Size of a variable of this type in bytes.
      size_t size = sizeof(vm::address_t);
      /// Index of the function type definition
      size_t type = 0;
      /// Allow this function to be inlined at the call site.
      bool allowInline = false;
      /// Expression that contains the function definition. Used to generate inline code
      size_t function_id = 0;
    };

    struct type_modifier {
      size_t base = 0;
      bool const_ = false;
      bool reference = false;
    };

    struct type {
      std::string identifier;
      std::variant<
        type_primitive,
        type_class,
        type_function,
        type_function_decl,
        type_modifier
      > desc;
    };

    bool is_integer(type_primitive const& primitive);
    bool is_bool(type_primitive const& primitive);
    bool is_void(type_primitive const& primitive);
    bool is_float(type_primitive const& primitive);
  }
}
