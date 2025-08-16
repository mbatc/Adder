#include "builtins.h"
#include "../ast.h"
#include "../program_builder.h"

namespace adder {
  namespace compiler {
    namespace builtin {
      bool i32_init_i64(program_builder* program) {
        auto & self = program->symbols[program->symbols.size() - 2];
        auto & arg  = program->symbols[program->symbols.size() - 1];
        std::optional<size_t> selfType = program->unwrap_type(self.type_index);
        if (!selfType.has_value()) {
          return false;
        }

        vm::register_index addr  = program->pin_symbol(self);
        vm::register_index value = program->pin_symbol(arg);

        program->store(value, addr, (uint8_t)program->get_type_size(selfType.value())); // Indirect store into address stored in `addr`
        program->release_register(value);
        program->release_register(addr);
        return true;
      }
    }

    size_t declare_i32(ast* tree) {
      expr::type_name selfTypeName;
      selfTypeName.name = "int32";

      expr::type_modifier selfType;
      selfType.reference = true;
      selfType.modified = tree->add(selfTypeName);

      expr::variable_declaration arg0;
      arg0.flags = symbol_flags::const_ | symbol_flags::fn_parameter;
      arg0.name = "self";
      arg0.type = tree->add(selfType);

      expr::type_name arg1TypeName;
      arg1TypeName.name = "int64";

      expr::variable_declaration arg1;
      arg1.flags = symbol_flags::const_ | symbol_flags::fn_parameter;
      arg1.name = "$1";
      arg1.type = tree->add(arg1TypeName);

      expr::byte_code code;
      code.callback = builtin::i32_init_i64;

      expr::type_name retTypeName;
      retTypeName.name = "void";

      expr::type_fn fnType;
      fnType.return_type = tree->add(retTypeName);
      fnType.argument_list.push_back(arg0.type.value());
      fnType.argument_list.push_back(arg1.type.value());

      expr::function_declaration decl;
      decl.arguments.push_back(tree->add(arg0));
      decl.arguments.push_back(tree->add(arg1));
      decl.body       = tree->add(code);
      decl.identifier = "";
      decl.type       = tree->add(fnType);
      decl.flags      = symbol_flags::initializer | symbol_flags::inline_;
      return tree->add(decl);
    }

    void define_builtins(ast* tree, expr::block *scope) {
      scope->statements.push_back(declare_i32(tree));
    }
  }
}

