#include "types.h"

namespace adder {
  namespace compiler {
    bool is_integer(type_primitive const& primitive)
    {
      return primitive >= type_primitive::int8
        && primitive <= type_primitive::uint64;
    }

    bool is_bool(type_primitive const& primitive)
    {
      return primitive == type_primitive::bool_;
    }

    bool is_void(type_primitive const& primitive)
    {
      return primitive == type_primitive::void_;
    }

    bool is_float(type_primitive const& primitive)
    {
      return primitive == type_primitive::float32 || 
        primitive == type_primitive::float64;
    }
  }
}
