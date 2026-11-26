#include "../context.h"
#include "compiler.h"
#include "compiler/program_metadata.h"

namespace adder {
  namespace compiler {
    bool find_type_names(context * compiler, ast const & tree, program_metadata * meta, size_t statementId);

    template<typename T>
    bool find_type_names(context * compiler, ast const & tree, program_metadata * meta, T const & decl, size_t statementId) {
      unused(decl);

      visit_sub_expressions(tree, statementId, [&](size_t child) { find_type_names(compiler, tree, meta, child); });

      auto & typeId = meta->statement_info[statementId].type_ref;
      typeId        = meta->get_type_reference(statementId);
      return true;
    }

    bool find_type_names(context * compiler, ast const & tree, program_metadata * meta, expr::class_decl const & decl,
                         size_t statementId) {
      std::string    name{decl.identifier};
      type_reference typeRef = meta->add_type(type{name, type_incomplete{}});

      meta->begin_namespace(program_metadata::ns{typeRef});

      visit_sub_expressions(tree, statementId, [&](size_t child) { find_type_names(compiler, tree, meta, child); });

      meta->end_namespace();

      auto & statementMeta   = meta->statement_info[statementId];
      statementMeta.type_ref = typeRef;
      return true;
    }

    bool find_type_names(context * compiler, program_metadata * meta, size_t id, expr::import_symbol const & import_stmt) {
      adder::unused(id);

      std::string                       name                 = (std::string)import_stmt.module_name;
      std::shared_ptr<program_metadata> imported_module_meta = compiler->get_module_metadata(meta->module_name, name);

      std::optional<type_reference> importedType = meta->get_type_reference(import_stmt.symbol_name);
      if (!importedType.has_value())
        return true;

      meta->add_type(importedType.value()); // TODO: Alias for imported type
      return true;
    }

    bool find_type_names(context * compiler, program_metadata * meta, size_t id, expr::import_module const & import_stmt) {
      adder::unused(id);

      std::string                       name                 = (std::string)import_stmt.module_name;
      std::shared_ptr<program_metadata> imported_module_meta = compiler->get_module_metadata(meta->module_name, name);
      for (size_t i = 0; i < imported_module_meta->types->size(); ++i) {
        meta->add_type({imported_module_meta.get(), type_id{i}}); // TODO: Alias for imported type
      }
      return true;
    }

    bool find_type_names(context * compiler, ast const & tree, program_metadata * meta, size_t statementId) {
      return  std::visit([&](auto const & exp) { return find_type_names(compiler, tree, meta, exp, statementId); },
                 tree.statements[statementId]);
    }

    std::optional<type_reference> evaluate_type_index(program_metadata * meta, size_t statementId) {
      std::optional<size_t> ret;
      auto &                typeId = meta->statement_info[statementId].type_ref;
      if (typeId.has_value()) {
        return typeId;
      }

      if (meta->tree.is<expr::type_modifier>(statementId)) {
        expr::type_modifier const & modifier = meta->tree.get<expr::type_modifier>(statementId);
        auto                        modified = evaluate_type_index(meta, modifier.modified);
        if (!modified.has_value()) {
          printf("Error: reference to unknown type\n");
          return std::nullopt;
        }
        type_modifier mod;
        mod.base      = modified.value();
        mod.const_    = modifier.const_;
        mod.reference = modifier.reference;

        type t;
        t.desc       = mod;
        t.identifier = get_type_name(meta->tree, statementId).value();
        typeId       = meta->add_type(t);
      }

      if (meta->tree.is<expr::type_fn>(statementId)) {
        expr::type_fn const & fn = meta->tree.get<expr::type_fn>(statementId);

        auto returnType = evaluate_type_index(meta, fn.return_type);
        if (!returnType.has_value()) {
          return std::nullopt;
        }

        type_function desc;
        desc.return_type = returnType.value();
        for (auto const & arg : fn.argument_list) {
          auto argType = evaluate_type_index(meta, arg);
          if (!argType.has_value()) {
            printf("Error: unknown type argument type\n");
            // TODO: Push error. Unable to evaluate argument type at index
            return std::nullopt;
          }
          desc.arguments.push_back(argType.value());
        }
        desc.func_type = fn.func_type;

        type t;
        t.desc       = desc;
        t.identifier = get_type_name(meta->tree, statementId).value();

        typeId = meta->add_type(t);
      }

      if (meta->tree.is<expr::class_decl>(statementId)) {
        // Parse class definition
        printf("Error: class decl not implemented\n");
        return std::nullopt;
      }

      if (!typeId.has_value()) {
        typeId = meta->get_type_reference(statementId);
      }

      return typeId;
    }

    bool complete_type_definitions(context * compiler, ast const & tree, program_metadata * meta, size_t statementId);

    template<typename T>
    bool complete_type_definitions(context * compiler, ast const & tree, program_metadata * meta, T const & decl,
                                   size_t statementId) {
      unused(decl);

      visit_sub_expressions(tree, statementId,
                            [&](size_t child) { complete_type_definitions(compiler, tree, meta, child); });

      auto & typeId = meta->statement_info[statementId].type_ref;
      typeId        = meta->get_type_reference(statementId);
      return true;
    }

    bool complete_type_definitions(context * compiler, ast const & tree, program_metadata * meta,
                                   expr::variable_declaration const & decl, size_t statementId) {
      if (!meta->current_namespace().type.has_value()) {
        visit_sub_expressions(tree, statementId,
                              [&](size_t child) { complete_type_definitions(compiler, tree, meta, child); });
        return true;
      }

      std::optional<type_reference> memberType = evaluate_type_index(meta, decl.type.value());
      if (!memberType.has_value()) {
        printf("Error: unknown type in member variable declaration\n");
        return false;
      }

      type_class::member m;
      m.flags      = decl.flags;
      m.type       = memberType.value();
      m.identifier = decl.name;

      type_reference classType = meta->current_namespace().type.value();
      type_class &   cls       = meta->types->at((size_t)classType.index).as<type_class>();
      cls.members.push_back(m);

      return true;
    }

    bool complete_type_definitions(context * compiler, ast const & tree, program_metadata * meta,
                                   expr::function_declaration const & decl, size_t statementId) {
      if (!meta->current_namespace().type.has_value()) {
        visit_sub_expressions(tree, statementId,
                              [&](size_t child) { complete_type_definitions(compiler, tree, meta, child); });
        return true;
      }

      // Declaration is within class definition. Add as a member
      std::optional<type_reference> memberType = evaluate_type_index(meta, decl.type.value());
      if (!memberType.has_value()) {
        printf("Error: unknown type in method declaration\n");
        return false;
      }

      type_class::member m;
      m.flags      = decl.flags;
      m.type       = memberType.value();
      m.identifier = decl.identifier;

      type_reference classType = meta->current_namespace().type.value();
      type_class &   cls       = meta->types->at((size_t)classType.index).as<type_class>();
      cls.members.push_back(m);

      return true;
    }

    bool complete_type_definitions(context * compiler, ast const & tree, program_metadata * meta,
                                   expr::class_decl const & decl, size_t statementId) {
      std::string    name{decl.identifier};
      type_reference typeRef = meta->add_type(type{name, type_incomplete{}});

      meta->begin_namespace(program_metadata::ns{typeRef});

      visit_sub_expressions(tree, statementId,
                            [&](size_t child) { complete_type_definitions(compiler, tree, meta, child); });

      meta->end_namespace();

      auto & statementMeta   = meta->statement_info[statementId];
      statementMeta.type_ref = typeRef;
      return true;
    }

    bool complete_type_definitions(context * compiler, ast const & tree, program_metadata * meta, size_t statementId) {
      return std::visit([&](auto const & exp) { return find_type_names(compiler, tree, meta, exp, statementId); },
                 tree.statements[statementId]);
    }

    bool evaluate_type_layout(type_reference const & ref) {
      constexpr size_t incomplete_size = (size_t)-1;

      if (ref.get_size() == incomplete_size) {
        printf("Error: Type definition has cyclic dependency.\n");
        return false;
      }

      if (ref.is_void() || ref.get_size() != 0)
        return true;

      auto & type = ref.meta->types->at((size_t)ref.index);
      return std::visit(adder::overloads{
                          [&](type_class &desc) {
                            desc.size = incomplete_size; // sentinel for "evaluating"

                            size_t alignment = 1;
                            size_t sz        = 0;
                            for (auto & member : desc.members) {
                              if (member.type.is_function()) {
                                continue;
                              }

                              if (!evaluate_type_layout(member.type)) {
                                return false;
                              }

                              size_t const memberSize = member.type.get_size();
                              member.offset           = sz + adder::bytes::calc_align_padding(memberSize, sz);
                              sz                      = member.offset + memberSize;
                              alignment               = std::max(alignment, std::min(memberSize, sizeof(max_align_t)));
                            }
                            sz += adder::bytes::calc_align_padding(alignment, sz);

                            desc.size = sz;

                            return ref.get_size() != 0;
                          },
                          [&](auto &) { return true; },
                        },
                        type.desc
      );
    }

    bool phase0_find_types(context * compiler, program_metadata * meta) {
      bool ok = true;
      visit_sub_expressions(meta->tree, meta->tree.statements.size() - 1,
                            [&](int64_t childId) { ok &= find_type_names(compiler, meta->tree, meta, childId); });
      visit_sub_expressions(meta->tree, meta->tree.statements.size() - 1,
                            [&](int64_t childId) { ok &= complete_type_definitions(compiler, meta->tree, meta, childId); });

      for (size_t idx = 0; idx < meta->types->size(); ++idx) {
        evaluate_type_layout({meta, type_id{idx}});
      }

      return ok;
    }
  } // namespace compiler
} // namespace adder
