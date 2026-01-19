#include "types.h"

namespace adder {
  namespace compiler {
    bool is_integer(type_primitive const& primitive) {
      return primitive >= type_primitive::int8
        && primitive <= type_primitive::uint64;
    }

    bool is_bool(type_primitive const& primitive) {
      return primitive == type_primitive::bool_;
    }

    bool is_void(type_primitive const& primitive) {
      return primitive == type_primitive::void_;
    }

    bool is_float(type_primitive const& primitive) {
      return primitive == type_primitive::float32 || 
        primitive == type_primitive::float64;
    }


    std::string_view get_primitive_type_name(type_primitive const & primitive) {
      switch (primitive) {
      case type_primitive::void_: return "void";
      case type_primitive::int8: return "int8";
      case type_primitive::int16: return "int16";
      case type_primitive::int32: return "int32";
      case type_primitive::int64: return "int64";
      case type_primitive::uint8: return "uint8";
      case type_primitive::uint16: return "uint16";
      case type_primitive::uint32: return "uint32";
      case type_primitive::uint64: return "uint64";
      case type_primitive::float32 : return "float32";
      case type_primitive::float64: return "float64";
      case type_primitive::bool_: return "bool";
      }
      return "";
    }
  }
}
