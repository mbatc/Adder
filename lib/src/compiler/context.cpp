#include "./context.h"
#include "compiler/lexer.h"
#include "compiler/ast.h"
#include "compiler/parser.h"
#include "compiler/ast/expressions.h"
#include "compiler/program_builder.h"

namespace adder {
  namespace compiler {
    std::shared_ptr<compiler::program_metadata> context::get_module_metadata(std::string const & source_module_name,
                                                                    std::string const & import_name) {
      if (import_name == "$builtins")
        return get_module_metadata(import_name);

      return get_module_metadata(vm->resolve_module_name(vm, source_module_name.c_str(), import_name.c_str()));
    }

    std::shared_ptr<compiler::program_metadata> context::get_module_metadata(std::string const & module_name) {
      auto it = imports.find(module_name);
      if (it != imports.end())
        return it->second;

      if (module_name == "$builtins")
        return compile_metadata(module_name, builtins_module());

      auto                                        source = vm->load_module_source(vm, module_name.c_str());
      std::shared_ptr<compiler::program_metadata> meta   = compile_metadata(module_name, source);
      return meta;
    }

    std::shared_ptr<compiler::program_metadata> context::compile_metadata(std::string const & module_uri,
                                                                 std::string const & source) {
      compiler::lexer::token_parser tokenizer(source);
      compiler::ast                 ast = compiler::parse(&tokenizer);
      if (!tokenizer.ok()) {
        for (auto & error : tokenizer.errors()) {
          printf("Error: %s\n", error.c_str());
        }
        return nullptr;
      }

      return compile_metadata(module_uri, std::move(ast), tokenizer.take_source());
    }

    std::shared_ptr<compiler::program_metadata> context::compile_metadata(std::string const & module_name,
                                                                 compiler::ast &&ast, std::string &&source) {
      auto meta = std::make_shared<compiler::program_metadata>();
      meta->tree        = std::move(ast);
      meta->source      = std::move(source);
      meta->module_name = module_name;
      meta->types       = &types;

      if (!evaluate_symbols(this, meta.get())) {
        return nullptr;
      }

      auto moduleInit = meta->find_symbol("()=>void:$module_init");
      if (moduleInit.has_value() && meta.get() == moduleInit->meta) {
        // assert(moduleInit.has_value() && "$module_init was not declared");
        meta->symbols[(size_t)moduleInit->index].function_root_scope_id = 0;
      }

      imports[module_name] = meta;

      return meta;
    }
    
    std::optional<program> context::compile(vm::machine * vm, std::string const & module_name) {
      if (vm->compiler == nullptr) {
        vm->compiler     = std::make_shared<compiler::context>();
        vm->compiler->vm = vm;
      }

      return phase3_generate_code(vm->compiler->get_module_metadata(module_name));
    }

    std::optional<program> context::compile(vm::machine * vm, std::string const & module_name, std::string const & source) {
      if (vm->compiler == nullptr) {
        vm->compiler = std::make_shared<compiler::context>();
        vm->compiler->vm = vm;
      }
    
      return phase3_generate_code(vm->compiler->compile_metadata(module_name, source));
    }
  }
}
