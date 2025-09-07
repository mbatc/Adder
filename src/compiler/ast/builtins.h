#pragma once

namespace adder {
  namespace compiler {
    namespace expr {
      struct block;
    }

    struct ast;
    struct program_builder;

    namespace builtin {
      bool int_init_int(program_builder * program);
    }

    void define_builtins(ast* tree, expr::block *scope);
  }
}
