#pragma once

namespace adder {
  namespace compiler {
    namespace expr {
      struct block;
    }

    struct ast;
    
    ast builtins_module();

    void import_builtins(ast * tree, expr::block * scope);
  }
}
