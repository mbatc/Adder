#pragma once

#include "compiler/program_metadata.h"

namespace adder {
  namespace vm {
    struct machine;
  }

  namespace compiler {
    bool phase0_find_types(context * compiler, program_metadata * meta);
    bool phase1_find_symbols();
    bool phase2_evaluate_types();
    std::optional<program> phase3_generate_code(std::shared_ptr<program_metadata> const & meta);

    bool evaluate_symbols(context * compiler, program_metadata * meta);
    /// Evaluate the type index of a ast statement that refers to a type
    std::optional<type_reference> evaluate_type_index(program_metadata * meta, size_t statementId);

    struct context {
      vm::machine *                                                      vm = nullptr;

      type_library                                                       types;
      std::map<std::string, std::shared_ptr<compiler::program_metadata>> imports;

      std::shared_ptr<compiler::program_metadata> get_module_metadata(std::string const & source_module_name,
                                                                      std::string const & import_name);
      std::shared_ptr<compiler::program_metadata> get_module_metadata(std::string const & module_name);

      std::shared_ptr<compiler::program_metadata> compile_metadata(std::string const & module_uri,
                                                                   std::string const & source);
      std::shared_ptr<compiler::program_metadata> compile_metadata(std::string const & module_name, compiler::ast && ast,
                                                                   std::string && source = {});
      
      std::optional<program> compile(vm::machine * vm, std::string const & module_name) {
        if (vm->compiler == nullptr) {
          vm->compiler     = std::make_shared<compiler::context>();
          vm->compiler->vm = vm;
        }

        return phase3_generate_code(vm->compiler->get_module_metadata(module_name));
      }

      std::optional<program> compile(vm::machine * vm, std::string const & module_name, std::string const & source) {
        if (vm->compiler == nullptr) {
          vm->compiler = std::make_shared<compiler::context>();
          vm->compiler->vm = vm;
        }
      
        return phase3_generate_code(vm->compiler->compile_metadata(module_name, source));
      }
    };
  }
}