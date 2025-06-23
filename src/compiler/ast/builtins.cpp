#include "builltins.h"
#include "../ast.h"
#include "../program_builder.h"

namespace adder {
  namespace compiler {
    namespace builtin {
      bool store(program_builder* program, vm::register_index src, program_offset const & addr, uint8_t sz) {
        vm::instruction instr;
        instr.code = vm::op_code::store_addr;
        instr.store.dst_addr = addr.offset;
        instr.store.src      = src;
        instr.store.size     = sz;
        program->add_instruction(instr);
      }

      bool store(program_builder* program, vm::register_index src, stack_offset const & addr, uint8_t sz) {
        vm::instruction instr;
        instr.code = vm::op_code::store_stack;
        instr.store_stack.offset = addr.offset;
        instr.store_stack.src    = src;
        instr.store_stack.size   = sz;
        program->add_instruction(instr);
      }

      bool store(program_builder* program, vm::register_index src, address_desc const & addr, uint8_t sz) {
        return std::visit([=](auto&& o) { return store(program, src, o, sz); }, addr);
      }

      bool i32_init_i64(program_builder* program) {
        auto & self = program->symbols[program->symbols.size() - 2];
        auto & arg  = program->symbols[program->symbols.size() - 1];

        vm::instruction mv;
        mv.code = vm::op_code::move;
        mv.move.dst = program->pin_register();
        mv.move.src = program->pin_symbol(arg);
        program->add_instruction(mv);

        if (self.address.has_value())
          store(program, mv.move.dst, self.address.value(), (uint8_t)type_size(program->types[self.type_index]));
        else {} // TODO: store, relocated.

        return true;
      }
    }

    void declare_i32(ast* tree)
    {
      expr::variable_declaration arg0;
      arg0.flags = symbol_flags::const_ | symbol_flags::fn_parameter;
      arg0.name = "$1";
      arg0.type_name = "int64";

      expr::identifier r;
      r.name = "void";

      expr::byte_code body;
      body.callback = builtin::i32_init_i64;

      expr::function_declaration decl;
      decl.body       = tree->add(body);
      decl.identifier = "";
      decl.signature  = "init:int32:int64";
      decl.flags      = symbol_flags::initializer;
      tree->add(decl);
    }

    void define_builtins(ast* tree)
    {
      declare_i32(tree);
    }
  }
}

