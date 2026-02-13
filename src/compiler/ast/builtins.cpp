#include "builtins.h"
#include "../ast.h"
#include "../program_builder.h"

namespace adder {
  namespace compiler {
    namespace builtin {
      bool int_init_int(program_builder * program) {
        auto self = program->find_value_by_identifier("self");
        auto arg  = program->find_value_by_identifier("a");
        if (!self.has_value() || !arg.has_value()) {
          return false;
        }

        std::optional<size_t> selfType = program->get_value_type(self.value());
        std::optional<size_t> argType = program->get_value_type(arg.value());
        if (!(program->meta.is_integer(program->meta.unwrap_type(selfType))
          && program->meta.is_integer(argType)))
          return false;

        vm::register_index addr  = program->load_address_of(self.value());
        vm::register_index value = program->load_value_of(arg.value());

        program->store(value, addr, (uint8_t)program->meta.get_type_size(selfType.value())); // Indirect store into address stored in `addr`
        program->release_register(value);
        program->release_register(addr);
        return true;
      }
      
      bool add_int_int(program_builder * program) {
        auto a = program->find_value_by_identifier("a");
        auto b = program->find_value_by_identifier("b");
        if (!a.has_value() || !b.has_value()) {
          return false;
        }

        size_t sz = program->meta.get_type_size(a->type_index.value());
        auto ret = program->get_return_value();
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
    }

    size_t init_int(ast* tree, size_t refType, std::string_view const & argType) {
      expr::variable_declaration arg0;
      arg0.flags = symbol_flags::const_ | symbol_flags::fn_parameter;
      arg0.name = "self";
      arg0.type = refType;

      expr::type_name arg1TypeName;
      arg1TypeName.name = argType;

      expr::variable_declaration arg1;
      arg1.flags = symbol_flags::const_ | symbol_flags::fn_parameter;
      arg1.name = "a";
      arg1.type = tree->add(arg1TypeName);

      expr::byte_code code;
      code.callback = builtin::int_init_int;

      expr::type_name retTypeName;
      retTypeName.name = "void";

      expr::type_fn fnType;
      fnType.return_type = tree->add(retTypeName);
      fnType.argument_list.push_back(arg0.type.value());
      fnType.argument_list.push_back(arg1.type.value());
      fnType.func_type = functor_type::initializer;

      expr::function_declaration decl;
      decl.arguments.push_back(tree->add(arg0));
      decl.arguments.push_back(tree->add(arg1));
      decl.body       = tree->add(code);
      decl.identifier = "";
      decl.type       = tree->add(fnType);
      decl.flags      = symbol_flags::inline_;
      
      return tree->add(decl);
    }
    
    size_t add_int(ast* tree, size_t valueType) {
      expr::variable_declaration arg0;
      arg0.flags = symbol_flags::const_ | symbol_flags::fn_parameter;
      arg0.name = "a";
      arg0.type = valueType;

      expr::variable_declaration arg1;
      arg1.flags = symbol_flags::const_ | symbol_flags::fn_parameter;
      arg1.name = "b";
      arg1.type = valueType;

      expr::byte_code code;
      code.callback = builtin::add_int_int;

      expr::type_fn fnType;
      fnType.return_type = valueType;
      fnType.argument_list.push_back(arg0.type.value());
      fnType.argument_list.push_back(arg1.type.value());
      fnType.func_type = functor_type::operator_;

      expr::function_declaration decl;
      decl.arguments.push_back(tree->add(arg0));
      decl.arguments.push_back(tree->add(arg1));
      decl.body       = tree->add(code);
      decl.identifier = "+";
      decl.type       = tree->add(fnType);
      decl.flags      = symbol_flags::inline_;

      return tree->add(decl);
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
          init_int(tree, refType, get_primitive_type_name(type_primitive::int64)),
          init_int(tree, refType, get_primitive_type_name(type_primitive::int32)),
          init_int(tree, refType, get_primitive_type_name(type_primitive::int16)),
          init_int(tree, refType, get_primitive_type_name(type_primitive::int8)),
          init_int(tree, refType, get_primitive_type_name(type_primitive::uint64)),
          init_int(tree, refType, get_primitive_type_name(type_primitive::uint32)),
          init_int(tree, refType, get_primitive_type_name(type_primitive::uint16)),
          init_int(tree, refType, get_primitive_type_name(type_primitive::uint8)),
          add_int(tree, valueType)
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

