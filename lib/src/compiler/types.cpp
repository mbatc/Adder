#include "compiler/types.h"
#include "compiler/program_metadata.h"

namespace adder {
  namespace compiler {
    bool is_incomplete(std::optional<type_reference> const & ref) {
      return ref.has_value() && ref->is<type_incomplete>();
    }

    bool is_integer(type_primitive const & primitive) {
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

    type_reference type_reference::undefined() {
      return {nullptr, type_id::undefined};
    }

    bool type_reference::operator==(const type_reference & rhs) const {

      return !(is_undefined() || rhs.is_undefined()) &&
              (rhs.meta->types == meta->types) && rhs.index == index;
    }

    bool type_reference::operator!=(const type_reference & rhs) const {
      return !operator==(rhs);
    }

    const type & type_reference::get_type() const {
      return meta->get(index);
    }

    std::string_view type_reference::get_identifier() const {
      if (is_undefined())
        return "";

      return meta->get(index).identifier;
    }

    size_t type_reference::get_size() const {
      return is_undefined() ? 0 : meta->get_type_size(index);
    }

    bool type_reference::is_undefined() const {
      return meta == nullptr || meta->types == nullptr || !meta->has(index);
    }

    std::optional<type_reference> type_reference::unwrap_type() const {
      if (!is<type_modifier>())
        return std::nullopt;
      return as<type_modifier>().base;
    }

    std::optional<type_reference> type_reference::decay_type() const {
      if (is<type_modifier>()) {
        const auto & modifier = as<type_modifier>();
        if (modifier.const_) {
          return modifier.base.decay_type();
        }
      }

      if (is<type_function_decl>())
        return as<type_function_decl>().type;

      return *this;
    }

    std::optional<type_reference> type_reference::remove_reference() const {
      if (!is<type_modifier>())
        return *this;

      const auto & modifier = as<type_modifier>();
      if (!modifier.reference)
        return *this;

      return modifier.base.decay_type();
    }

    std::optional<type_reference> type_reference::return_type() const {
      if (is<type_function>()) {
        return as<type_function>().return_type;
      }

      if (is<type_function_decl>()) {
        return as<type_function_decl>().type.return_type();
      }

      auto unwrapped = unwrap_type();
      if (unwrapped.has_value())
        return unwrapped->return_type();

      return std::nullopt;
    }

    bool type_reference::is_reference_of(std::optional<type_reference> const & baseType) const {
      return baseType.has_value() && is_reference() && remove_reference() == baseType;
    }

    bool type_reference::is_reference() const {
      return is<type_modifier>() && as<type_modifier>().reference;
    }

    bool type_reference::is_function_decl() const {
      return is<type_function_decl>();
    }

    bool type_reference::is_function() const {
      return is<type_function_decl>() || is<type_function>() || compiler::is_function(unwrap_type());
    }

    functor_type type_reference::get_functor_type() const {
      auto decayed = decay_type();
      if (!(decayed.has_value() && decayed->is<type_function>()))
        return functor_type::none;

      auto & decl = decayed->as<type_function>();
      return decl.func_type;
    }

    bool type_reference::is_const() const {
      return !is<type_modifier>() && as<type_modifier>().const_;
    }

    bool type_reference::is_integer() const {
      return is<type_primitive>() && compiler::is_integer(as<type_primitive>());
    }

    bool type_reference::is_float() const {
      return is<type_primitive>() && compiler::is_float(as<type_primitive>());
    }

    bool type_reference::is_bool() const {
      return is<type_primitive>() && compiler::is_bool(as<type_primitive>());
    }

    bool type_reference::is_void() const {
      return is<type_primitive>() && compiler::is_void(as<type_primitive>());
    }

    bool type_reference::is_valid_function_overload(std::optional<type_reference> const & b) const {
      if (!(is_function() && compiler::is_function(b))) {
        return false;
      }

      auto decayedA = decay_type();
      auto decayedB = b->decay_type();
      if (!decayedA.has_value() || !decayedB.has_value()) {
        return false;
      }

      return as<type_function>().arguments != decayedB->as<type_function>().arguments;
    }

    
    const type & get_type(std::optional<type_reference> const & ref) {
      return ref->get_type();
    }

    std::string_view get_identifier(std::optional<type_reference> const & ref) {
      return ref.has_value() ? ref->get_identifier() : "";
    }

    size_t get_size(std::optional<type_reference> const & ref) {
      return ref.has_value() ? ref->get_size() : 0;
    }

    bool is_undefined(std::optional<type_reference> const & ref) {
      return !ref.has_value() || ref->is_undefined();
    }

    std::optional<type_reference> unwrap_type(std::optional<type_reference> const& ref) {
      return ref.has_value() ? ref->unwrap_type() : std::nullopt;
    }

    std::optional<type_reference> decay_type(std::optional<type_reference> const & ref) {
      return ref.has_value() ? ref->decay_type() : std::nullopt;
    }

    std::optional<type_reference> remove_reference(std::optional<type_reference> const & ref) {
      return ref.has_value() ? ref->remove_reference() : std::nullopt;
    }

    std::optional<type_reference> return_type_of(std::optional<type_reference> const & ref) {
      return ref.has_value() ? ref->return_type() : std::nullopt;
    }

    bool is_reference_of(std::optional<type_reference> const & refType, std::optional<type_reference> const & baseType) {
      return refType.has_value() && refType->is_reference_of(baseType);
    }

    bool is_reference(std::optional<type_reference> const& ref) {
      return ref.has_value() && ref->is_reference();
    }

    bool is_const(std::optional<type_reference> const & ref) {
      return ref.has_value() && ref->is_const();
    }

    bool is_function_decl(std::optional<type_reference> const & ref) {
      return ref.has_value() && ref->is_function_decl();
    }

    bool is_function(std::optional<type_reference> const & ref) {
      return ref.has_value() && ref->is_function();
    }

    functor_type get_functor_type(std::optional<type_reference> const & ref) {
      return ref.has_value() ? ref->get_functor_type() : functor_type::none;
    }

    bool is_integer(std::optional<type_reference> const & ref) {
      return ref.has_value() && ref->is_integer();
    }

    bool is_float(std::optional<type_reference> const & ref) {
      return ref.has_value() && ref->is_float();
    }

    bool is_bool(std::optional<type_reference> const & ref) {
      return ref.has_value() && ref->is_bool();
    }

    bool is_void(std::optional<type_reference> const & ref) {
      return ref.has_value() && ref->is_void();
    }

    bool is_valid_function_overload(std::optional<type_reference> const & a, std::optional<type_reference> const & b) {
      return a.has_value() && a->is_valid_function_overload(b);
    }
  }
}
