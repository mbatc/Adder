#include "compiler.h"
#include "compiler/program_metadata.h"
#include "../context.h"

namespace adder {
  namespace compiler {
    bool find_types(context * compiler, ast const & tree, program_metadata * meta, size_t statementId);
    
    template<typename T>
    bool find_types(context * compiler, ast const & tree, program_metadata * meta, T const & decl, size_t statementId) {
      unused(decl);
      visit_sub_expressions(tree, statementId, [&](size_t child) { find_types(compiler, tree, meta, child); });
      
      auto& typeId = meta->statement_info[statementId].type_ref;
      typeId = meta->get_type_reference(statementId);
      return true;
    }
    
    bool find_types(context * compiler, ast const & tree, program_metadata * meta, expr::class_decl const & decl, size_t statementId) {
      type_reference typeRef = meta->add_type(type{std::string(decl.identifier), type_incomplete{}});
      if (!is_incomplete(typeRef)) {
        printf("Class '%.*s' already defined\n", (int)decl.identifier.length(), decl.identifier.data());
        return false;
      }

      type_class cls;
      cls.members;
      if (decl.destroy.has_value()) {
        type_class::member dtor;
        dtor.identifier = "";
        dtor.flags      = symbol_flags::function;
        if (!find_types(compiler, tree, meta, decl.destroy.value()))
          return false;
      }

      for (size_t method : decl.methods) {
        if (!find_types(compiler, tree, meta, method))
          return false;
      }

      for (size_t member : decl.members) {
        if (!find_types(compiler, tree, meta, member))
          return false;
      }

      cls.size = 0;

      // Complete the type definition
      meta->types->at((size_t)typeRef.index).desc = cls;

      auto & statementMeta   = meta->statement_info[statementId];
      statementMeta.type_ref = typeRef;
      return !(is_undefined(typeRef) || is_incomplete(typeRef));
    }

    bool find_types(context* compiler, ast const& tree, program_metadata* meta, expr::type_modifier const& decl,
      size_t statementId) {
      expr::type_modifier const & modifier = meta->tree.get<expr::type_modifier>(statementId);
      auto modified = evaluate_type_index(meta, modifier.modified);
      if (!modified.has_value()) {
        printf("Error: reference to unknown type\n");
        return false;
      }
      type_modifier mod;
      mod.base = modified.value();
      mod.const_ = modifier.const_;
      mod.reference = modifier.reference;
      
      type t;
      t.desc = mod;
      t.identifier = get_type_name(meta->tree, statementId).value();

      meta->statement_info[statementId].type_ref = meta->add_type(t);

      return !is_undefined(meta->statement_info[statementId].type_ref);
    }

    bool find_types(context * compiler, ast const & tree, program_metadata * meta, expr::type_fn const & decl,
      size_t statementId) {
      expr::type_fn const & fn = meta->tree.get<expr::type_fn>(statementId);

      auto returnType = evaluate_type_index(meta, fn.return_type);
      if (!returnType.has_value()) {
        return false;
      }

      type_function desc;
      desc.return_type = returnType.value();
      for (auto const & arg : fn.argument_list) {
        auto argType = evaluate_type_index(meta, arg);
        if (!argType.has_value()) {
          printf("Error: unknown type argument type\n");
          // TODO: Push error. Unable to evaluate argument type at index
          return false;
        }
        desc.arguments.push_back(argType.value());
      }
      desc.func_type = fn.func_type;

      type t;
      t.desc = desc;
      t.identifier = get_type_name(meta->tree, statementId).value();
      
      auto& typeId = meta->statement_info[statementId].type_ref;
      typeId = meta->add_type(t);

      return !is_undefined(typeId);
    }
    
    bool find_types(context * compiler, program_metadata * meta, size_t id,
                                    expr::import_symbol const & import_stmt) {
      std::string                       name                 = (std::string)import_stmt.module_name;
      std::shared_ptr<program_metadata> imported_module_meta = compiler->get_module_metadata(meta->module_name, name);

      std::optional<type_reference> importedType = meta->get_type_reference(import_stmt.symbol_name);
      if (!importedType.has_value())
        return true;

      meta->add_type(importedType.value()); // TODO: Alias for imported type
      return true;
    }

    bool find_types(context * compiler, program_metadata * meta, size_t id,
                                    expr::import_module const & import_stmt) {
      std::string                       name                 = (std::string)import_stmt.module_name;
      std::shared_ptr<program_metadata> imported_module_meta = compiler->get_module_metadata(meta->module_name, name);
      for (size_t i = 0; i < imported_module_meta->types->size(); ++i) {
        meta->add_type({imported_module_meta.get(), type_id{i}}); // TODO: Alias for imported type
      }
      return true;
    }
    
    bool find_types(context * compiler, ast const & tree, program_metadata * meta, size_t statementId) {
      std::visit([&](auto const & exp) { return find_types(compiler, tree, meta, exp, statementId); },
                        tree.statements[statementId]);
    }
    
    bool phase0_find_types(context * compiler, program_metadata * meta) {
      // TODO: User types first?
      expr::block const & top = meta->tree.get<expr::block>(meta->tree.statements.size() - 1);
      for (size_t statementId : top.statements) {
        if (!find_types(compiler, meta->tree, meta, statementId)) {
          return false;
        }
      }

      // Eval type sizes
      // for (auto & type : )

      return true;
    }
  }
}
