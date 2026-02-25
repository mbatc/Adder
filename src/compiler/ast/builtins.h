#pragma once

namespace adder {
  namespace compiler {
    namespace expr {
      struct block;
    }

    struct ast;
    struct program_builder;

    void define_builtins(ast* tree, expr::block *scope);
  }
}
