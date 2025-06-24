#include "builtins.h"
#include "../ast.h"
#include "../program_builder.h"

namespace adder {
  namespace compiler {
    namespace builtin {
      bool i32_init_i64(program_builder* program) {
        auto & self = program->symbols[program->symbols.size() - 2];
        auto & arg  = program->symbols[program->symbols.size() - 1];

        vm::instruction mv;
        mv.code = vm::op_code::move;
        mv.move.dst = program->pin_register();
        mv.move.src = program->pin_symbol(arg);
        program->add_instruction(mv);
        program->store(mv.move.dst, self);
        return true;
      }
    }

    size_t declare_i32(ast* tree)
    {
      expr::variable_declaration arg0;
      arg0.flags = symbol_flags::const_ | symbol_flags::fn_parameter;
      arg0.name = "self";
      arg0.type_name = "int32";

      expr::variable_declaration arg1;
      arg1.flags = symbol_flags::const_ | symbol_flags::fn_parameter;
      arg1.name = "$1";
      arg1.type_name = "int64";

      expr::byte_code code;
      code.callback = builtin::i32_init_i64;

      expr::function_declaration decl;
      decl.arguments.push_back(tree->add(arg0));
      decl.body       = tree->add(code);
      decl.identifier = "";
      decl.signature  = "init:[ref]int32,int64:";
      decl.flags      = symbol_flags::initializer;
      return tree->add(decl);
    }

    void define_builtins(ast* tree, expr::block *scope)
    {
      scope->statements.push_back(declare_i32(tree));
    }
  }
}

