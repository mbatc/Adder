#include "compiler.h"
#include "vm.h"
#include "compiler/context.h"


namespace adder {
  std::optional<program> compile(vm::machine * vm, std::string const & module_name) {
    if (vm->compiler == nullptr) {
      vm->compiler     = std::make_shared<compiler::context>();
      vm->compiler->vm = vm;
    }

    return phase3_generate_code(vm->compiler->get_module_metadata(module_name));
  }

  std::optional<program> compile(vm::machine * vm, std::string const & module_name, std::string const & source) {
    if (vm->compiler == nullptr) {
      vm->compiler     = std::make_shared<compiler::context>();
      vm->compiler->vm = vm;
    }

    return phase3_generate_code(vm->compiler->compile_metadata(module_name, source));
  }
} // namespace adder
