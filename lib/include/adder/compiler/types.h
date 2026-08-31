#pragma once

#include "../common.h"
#include "../vm.h"

#include <variant>
#include <optional>

namespace adder {
  // Compiler implementation
  namespace compiler {
    struct program_metadata;
    enum class symbol_flags {
      none         = 0,
      const_       = 1 << 0, ///< Symbol can not be mutated.
      extern_      = 1 << 1, ///< External to the program. Symbol is located by the host.
      import_      = 1 << 3, ///< Symbol imported from another module.
      fn_parameter = 1 << 4, ///< This variable is a function parameter
      static_      = 1 << 5, ///< This symbol has static storage
      inline_      = 1 << 6, ///< This symbol can be inlined where possible
      function     = 1 << 7, ///< This symbol is a function definition
    };
  }

  template<>
  struct enable_bitwise_ops<compiler::symbol_flags> : std::true_type {};

  namespace compiler {
    struct program_metadata;
    struct type;

    enum class type_primitive;
    enum class functor_type;

    enum class type_id : size_t {
      undefined = -1,
    };

    struct type_reference {
      program_metadata const * meta  = nullptr;
      type_id                  index = type_id::undefined;

      static type_reference undefined();

      bool operator==(const type_reference & rhs) const;
      bool operator!=(const type_reference & rhs) const;

      const type &     get_type() const;
      std::string_view get_identifier() const;
      size_t           get_size() const;

      bool is_undefined() const;

      std::optional<type_reference> unwrap_type() const;
      std::optional<type_reference> decay_type() const;
      std::optional<type_reference> remove_reference() const;
      std::optional<type_reference> return_type() const;

      bool         is_reference_of(std::optional<type_reference> const & baseType) const;
      bool         is_reference() const;
      bool         is_const() const;
      bool         is_function_decl() const;
      bool         is_function() const;
      functor_type get_functor_type() const;
      bool         is_integer() const;
      bool         is_float() const;
      bool         is_bool() const;
      bool         is_void() const;

      bool is_valid_function_overload(std::optional<type_reference> const & b) const;

      template<typename T>
      bool is() const {
        return !is_undefined() && get_type().is<T>();
      }

      template<typename T>
      T const & as() const {
        return get_type().as<T>();
      }

      template<typename T>
      T const* try_get_as() const {
        return is<T>() ? &as<T>() : nullptr;
      }
    };

    
    struct type_incomplete {};

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
      count,
    };

    /// Type of function declaraition
    enum class functor_type {
      none,
      free,
      member,
      initializer,
      destructor,
      operator_,
    };

    struct type_class {
      struct member {
        std::string    identifier;
        symbol_flags   flags;
        type_reference type;
        size_t         offset;
      };

      std::vector<member> members;

      size_t size = 0; ///< Size of the class in bytes
    };

    struct type_function {
      /// Size of a variable of this type in bytes.
      size_t size = sizeof(vm::address_t);
      /// Index of the return type definition
      type_reference return_type = type_reference::undefined();
      /// Indices of argument type definitions
      std::vector<type_reference> arguments;
      /// Type of method.
      functor_type func_type = functor_type::free;
    };
    
    struct type_function_decl {
      /// Size of a variable of this type in bytes.
      size_t size = sizeof(vm::address_t);
      /// Index of the function type definition
      type_reference type = type_reference::undefined();
      /// Allow this function to be inlined at the call site.
      bool allowInline = false;
      /// Expression that contains the function definition. Used to generate inline code
      size_t function_id = 0;
    };

    struct type_modifier {
      type_reference base = type_reference::undefined();
      bool    const_      = false;
      bool    reference   = false;
    };

    struct type {
      using descriptor = std::variant<type_primitive, type_class, type_function, type_function_decl, type_modifier, type_incomplete>;
      std::string identifier;
      descriptor  desc;

      template<typename T>
      T & as() {
        return std::get<T>(desc);
      }

      template<typename T>
      T const & as() const {
        return std::get<T>(desc);
      }

      template<typename T>
      bool is() const {
        return std::holds_alternative<T>(desc);
      }
    };

    bool is_integer(type_primitive const & primitive);
    bool is_bool(type_primitive const & primitive);
    bool is_void(type_primitive const & primitive);
    bool is_float(type_primitive const & primitive);

    std::string_view get_primitive_type_name(type_primitive const & primitive);

    const type &     get_type(std::optional<type_reference> const & ref);
    std::string_view get_identifier(std::optional<type_reference> const & ref);
    size_t           get_size(std::optional<type_reference> const & ref);

    bool is_undefined(std::optional<type_reference> const & ref);
    bool is_incomplete(std::optional<type_reference> const & ref);

    std::optional<type_reference> unwrap_type(std::optional<type_reference> const & refs);
    std::optional<type_reference> decay_type(std::optional<type_reference> const & ref);
    std::optional<type_reference> remove_reference(std::optional<type_reference> const & ref);
    std::optional<type_reference> return_type_of(std::optional<type_reference> const & ref);

    bool         is_reference_of(std::optional<type_reference> const & refType,
                                 std::optional<type_reference> const & baseType);
    bool         is_reference(std::optional<type_reference> const & ref);
    bool         is_const(std::optional<type_reference> const & ref);
    bool         is_function_decl(std::optional<type_reference> const & ref);
    bool         is_function(std::optional<type_reference> const & ref);
    functor_type get_functor_type(std::optional<type_reference> const & ref);
    bool         is_integer(std::optional<type_reference> const & ref);
    bool         is_float(std::optional<type_reference> const & ref);
    bool         is_bool(std::optional<type_reference> const & ref);
    bool         is_void(std::optional<type_reference> const & ref);

    bool is_valid_function_overload(std::optional<type_reference> const & a, std::optional<type_reference> const & b);

  }
}
