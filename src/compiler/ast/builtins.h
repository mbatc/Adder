#pragma once

namespace adder {
  namespace compiler {
    namespace expr {
      struct block;
    }

    struct ast;
    struct program_builder;

    namespace builtin {
      bool i32_init_i64(program_builder * program);
    }

    void define_builtins(ast* tree, expr::block *scope);
  }
}
