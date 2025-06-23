#include "types.h"

namespace adder {
  namespace compiler {
    size_t type_size(type_primitive const& desc) {
      switch (desc) {
      case type_primitive::_void: return 0;
      case type_primitive::int8: return sizeof(int8_t);
      case type_primitive::int16: return sizeof(int16_t);
      case type_primitive::int32: return sizeof(int32_t);
      case type_primitive::int64: return sizeof(int64_t);
      case type_primitive::uint8: return sizeof(uint8_t);
      case type_primitive::uint16: return sizeof(uint16_t);
      case type_primitive::uint32: return sizeof(uint32_t);
      case type_primitive::uint64: return sizeof(uint64_t);
      case type_primitive::float32: return sizeof(float);
      case type_primitive::float64: return sizeof(double);
      case type_primitive::bool_: return sizeof(bool);
      }
      return 0;
    }

    size_t type_size(class_desc const& desc) {
      return desc.size;
    }

    size_t type_size(function_desc const& desc) {
      return desc.size;
    }

    size_t type_size(type const& type) {
      return std::visit([](auto const& o) {
        return type_size(o);
        }, type.desc);
    }
  }
}
