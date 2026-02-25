#include "builtins.h"
#include "../ast.h"
#include "../program_builder.h"

namespace adder {
  namespace compiler {
    namespace builtin {
      bool init_int_int(program_builder * program) {
        auto self = program->find_value_by_identifier("self");
        auto arg  = program->find_value_by_identifier("a");
        if (!self.has_value() || !arg.has_value()) {
          return false;
        }

        std::optional<size_t> selfType = program->get_value_type(self.value());
        // std::optional<size_t> argType = program->get_value_type(arg.value());
        // if (!(program->meta.is_integer(program->meta.unwrap_type(selfType))
        //   && program->meta.is_integer(argType)))
        //   return false;

        const uint8_t sz = (uint8_t)program->meta.get_type_size(selfType.value());
        vm::register_index addr = program->load_value_of(self.value()); // self is a reference, so load the address it points to.
        if (arg.value().constant.has_value()) {
          program->store_constant(arg.value().constant.value(), addr, sz);
        }
        else {
          vm::register_index value = program->load_value_of(arg.value());
          program->store(value, addr, sz);
          program->release_register(value);
        }
        program->release_register(addr);
        return true;
      }

      bool init_int_float(program_builder * program) {
        
      }

      bool int_assign_int(program_builder * program) {
        // vm::register_index lhs  = program->load_value_of(a.value());
        // vm::register_index rhs  = program->load_value_of(b.value());
        // vm::register_index addr = program->load_address_of(ret);
        // program->addi(lhs, lhs, rhs);
        // program->store(lhs, addr, (uint8_t)sz);
        // 
        // program->release_register(addr);
        // program->release_register(rhs);
        // program->release_register(lhs);
      }

      bool add_int_int(program_builder * program) {
        auto a = program->find_value_by_identifier("a");
        auto b = program->find_value_by_identifier("b");
        if (!a.has_value() || !b.has_value()) {
          return false;
        }

        auto ret = program->get_return_value();
        // if (a->constant.has_value() && b->constant.has_value()) {
        //   const uint8_t sz = (uint8_t)program->meta.get_type_size(addr);
        //   program->store_constant(a->constant.value() + b->constant.value(), addr, sz);
        // }

        // Could abstract 'register_index' with 'register/instruction_operand' 
        // This might store a register index + optional offset along with if it is indirect or not.
        size_t sz = program->meta.get_type_size(a->type_index.value());
        vm::register_index lhs  = program->load_value_of(a.value());
        vm::register_index rhs  = program->load_value_of(b.value());
        vm::register_index addr = program->load_address_of(ret);
        program->addi(lhs, lhs, rhs);
        program->store(lhs, addr, (uint8_t)sz);

        program->release_register(addr);
        program->release_register(rhs);
        program->release_register(lhs);
        return true;
      }

      bool eq_int_int(program_builder * program) {

      }

      bool gt_int_int(program_builder * program) {

      }

      bool lt_int_int(program_builder * program) {

      }

      bool le_int_int(program_builder * program) {

      }

      bool ge_int_int(program_builder * program) {

      }
    }

    struct parameter_desc {
      std::string_view name;
      std::variant<size_t, std::string_view> type;
    };

    struct builtin_desc {
      functor_type flavour;
      std::string_view identifier;
      std::variant<size_t, std::string_view> return_type;
      std::vector<parameter_desc> args;
      bool (*bytecode)(program_builder*);
    };

    size_t declare_type(ast* tree, std::variant<size_t, std::string_view> const & type) {
      if (std::holds_alternative<size_t>(type)) {
        return std::get<size_t>(type);
      }
      expr::type_name typeName;
      typeName.name = std::get<std::string_view>(type);
      return tree->add(typeName);
    }

    size_t declare_func(ast * tree, builtin_desc const & func) {
      expr::function_declaration decl;
      expr::type_fn fnType;
      for (auto& arg : func.args) {
        auto argType = declare_type(tree, arg.type);
        decl.arguments.push_back(argType);

        expr::variable_declaration decl;
        decl.flags = symbol_flags::const_ | symbol_flags::fn_parameter;
        decl.name = arg.name;
        decl.type = argType;
        fnType.argument_list.push_back(tree->add(decl));
      }
      fnType.return_type = declare_type(tree, func.return_type);
      fnType.func_type   = func.flavour;

      expr::byte_code code;
      code.callback = func.bytecode;

      decl.identifier = func.identifier;
      decl.flags      = symbol_flags::inline_;
      decl.type       = tree->add(fnType);
      decl.body       = tree->add(code);
    }

    size_t init_int(ast* tree, size_t refType, std::string_view const & argType) {
    }
    size_t declare_initializer(ast* tree, size_t selfType, std::variant<size_t, std::string_view> argType, bool (*bytecode)(program_builder*)) {
      return declare_func(tree, {
        functor_type::initializer,
        "",
        "void", {
          { "self", selfType },
          { "a",    argType },
        },
        bytecode
      });
    }

    size_t declare_binary_operator(ast* tree, expr::operator_type op, size_t valueType, bool (*bytecode)(program_builder*)) {
      return declare_func(tree, {
        functor_type::operator_,
        expr::get_operator_identifer(op),
        valueType,
        {
          { "a", valueType },
          { "b", valueType },
        },
        bytecode
      });
    }

    size_t add_int(ast* tree, size_t valueType) {
    }

    void declare_integer(ast* tree, expr::block *scope, type_primitive primitive) {
      expr::type_name selfTypeName;
      selfTypeName.name = get_primitive_type_name(primitive);

      expr::type_modifier selfType;
      selfType.reference = true;
      selfType.modified = tree->add(selfTypeName);

      const size_t valueType = tree->add(selfTypeName);
      const size_t refType = tree->add(selfType);

      scope->statements.insert(
        scope->statements.end(),
        {
          declare_initializer(tree, refType, get_primitive_type_name(type_primitive::int64), builtin::init_int_int),
          declare_initializer(tree, refType, get_primitive_type_name(type_primitive::int32), builtin::init_int_int),
          declare_initializer(tree, refType, get_primitive_type_name(type_primitive::int16), builtin::init_int_int),
          declare_initializer(tree, refType, get_primitive_type_name(type_primitive::int8), builtin::init_int_int),
          declare_initializer(tree, refType, get_primitive_type_name(type_primitive::uint64), builtin::init_int_int),
          declare_initializer(tree, refType, get_primitive_type_name(type_primitive::uint32), builtin::init_int_int),
          declare_initializer(tree, refType, get_primitive_type_name(type_primitive::uint16), builtin::init_int_int),
          declare_initializer(tree, refType, get_primitive_type_name(type_primitive::uint8), builtin::init_int_int),
          declare_initializer(tree, refType, get_primitive_type_name(type_primitive::float32), builtin::init_int_float),
          declare_initializer(tree, refType, get_primitive_type_name(type_primitive::float64), builtin::init_int_float),
          declare_binary_operator(tree, expr::operator_type::add, valueType, builtin::add_int_int)
        }
      );
    }

    void define_builtins(ast* tree, expr::block *scope) {
      declare_integer(tree, scope, type_primitive::int64);
      declare_integer(tree, scope, type_primitive::int32);
      declare_integer(tree, scope, type_primitive::int16);
      declare_integer(tree, scope, type_primitive::int8);
      declare_integer(tree, scope, type_primitive::uint64);
      declare_integer(tree, scope, type_primitive::uint32);
      declare_integer(tree, scope, type_primitive::uint16);
      declare_integer(tree, scope, type_primitive::uint8);
    }
  }
}

