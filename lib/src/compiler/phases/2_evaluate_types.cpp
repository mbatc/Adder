#include "../context.h"

namespace adder {
  namespace compiler {
    struct symbol_eval_context {
      std::optional<size_t> function_root_scope_id = 0;
      size_t                scope_id               = 0;
      bool                  is_call                = false;
      std::optional<size_t> call_parameter_list;
    };

    type_reference eval_decltype(program_metadata * meta, size_t statementId);

    template<typename T>
    type_reference eval_decltype_impl(program_metadata * meta, size_t statementId, T const & o) {
      unused(o);

      auto metaTypeId = meta->statement_info[statementId].type_ref;
      return metaTypeId.value_or(meta->get_type_reference(get_primitive_type_name(type_primitive::void_)).value());
    }

    type_reference eval_decltype_impl(program_metadata * meta, size_t statementId, expr::identifier const & identifier) {
      unused(identifier);

      auto symbolIndex = meta->statement_info[statementId].symbol_index;
      if (!symbolIndex.has_value()) {
        return meta->get_type_reference(get_primitive_type_name(type_primitive::void_)).value();
      }
      return meta->symbols[symbolIndex.value()].type;
    }

    type_reference eval_decltype_impl(program_metadata * meta, size_t statementId, expr::binary_operator const & op) {
      unused(statementId);
      // meta->is_valid_function_overload();
      if (op.type_name == expr::operator_type::call) {
        type_reference funcType = eval_decltype(meta, op.left.value());
        assert(funcType.is_function());
        auto returnType = funcType.return_type();
        return returnType.value_or(type_reference::undefined());
      } else {
        // TODO: Get operator method and eval return type
      }
      return type_reference::undefined();
    }

    type_reference eval_decltype(program_metadata * meta, size_t statementId) {
      return std::visit([&](auto && o) { return eval_decltype_impl(meta, statementId, o); },
                        meta->tree.statements[statementId]);
    }

    template<typename T>
    bool evaluate_statement_symbols(context * compiler, program_metadata * meta, size_t id, T const & other,
                                    symbol_eval_context const & ctx) {
      unused(other);

      bool result = true;
      visit_sub_expressions(meta->tree, id,
                            [&](size_t childId) { result = evaluate_symbols(compiler, meta, childId, ctx); });
      return result;
    }

    bool evaluate_statement_symbols(context * compiler, program_metadata * meta, size_t id, expr::class_decl const & decl,
                                    symbol_eval_context const & ctx) {
      unused(compiler, id, decl, ctx);

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
        if (!evaluate_symbols(compiler, meta, decl.destroy.value(), ctx))
          return false;
      }

      for (size_t method : decl.methods) {
        if (!evaluate_symbols(compiler, meta, method, ctx))
          return false;
      }

      for (size_t member : decl.members) {
        if (!evaluate_symbols(compiler, meta, member, ctx))
          return false;
      }

      cls.size = 0;
      // for (type_class::member & member : cls.members) {
      //   if (member.flags) {
      //
      //   }
      // }

      // Complete the type definition
      meta->types->at((size_t)typeRef.index).desc = cls;

      auto & statementMeta   = meta->statement_info[id];
      statementMeta.type_ref = typeRef;
      return !(is_undefined(typeRef) || is_incomplete(typeRef));
    }

    bool evaluate_statement_symbols(context * compiler, program_metadata * meta, size_t id,
                                    expr::type_declaration const & decl, symbol_eval_context const & ctx) {
      unused(compiler, id, decl, ctx);

      auto & statementMeta   = meta->statement_info[id];
      statementMeta.type_ref = meta->add_type(type{std::string(decl.identifier), decl.desc});
      return !is_undefined(statementMeta.type_ref);
    }

    bool evaluate_statement_symbols(context * compiler, program_metadata * meta, size_t id,
                                    expr::identifier const & identifier, symbol_eval_context const & ctx) {
      unused(compiler);
      std::optional<symbol_reference> symbol;
      if (ctx.is_call) {
        symbol = meta->search_for_callable_symbol(ctx.scope_id, identifier.name, ctx.call_parameter_list);
      } else {
        symbol = meta->search_for_symbol(ctx.scope_id, identifier.name);
      }

      if (!symbol.has_value()) {
        printf("Error: Unable to resolve identifier '%.*s'\n", (int)identifier.name.length(), identifier.name.data());
        return false;
      }

      auto & statementMeta = meta->statement_info[id];
      auto   it            = std::find(meta->symbol_references.begin(), meta->symbol_references.end(), symbol.value());
      // TODO: More elegant way of mapping symbols to statements.
      statementMeta.symbol_index = it - meta->symbol_references.begin();
      statementMeta.type_ref     = symbol->get_type();
      return statementMeta.symbol_index.has_value();
    }

    bool evaluate_literal_symbols(program_metadata * meta, size_t id, bool value, symbol_eval_context const & ctx) {
      unused(value, ctx);
      meta->statement_info[id].type_ref = meta->get_type_reference(type_primitive::bool_);
      return true;
    }

    bool evaluate_literal_symbols(program_metadata * meta, size_t id, int64_t value, symbol_eval_context const & ctx) {
      unused(value, ctx);
      meta->statement_info[id].type_ref = meta->get_type_reference(type_primitive::int64);
      return true;
    }

    bool evaluate_literal_symbols(program_metadata * meta, size_t id, double value, symbol_eval_context const & ctx) {
      unused(value, ctx);
      meta->statement_info[id].type_ref = meta->get_type_reference(type_primitive::float64);
      return true;
    }

    bool evaluate_literal_symbols(program_metadata * meta, size_t id, std::string_view const & value,
                                  symbol_eval_context const & ctx) {
      unused(meta, id, value, ctx);
      assert(false && "Not implemented");
      // TODO: Implement strings
      // meta->statement_info[id].type_id = meta->get_type_index(get_primitive_type_name(type_primitive::));
      return true;
    }

    bool evaluate_statement_symbols(context * compiler, program_metadata * meta, size_t id, expr::literal const & decl,
                                    symbol_eval_context const & ctx) {
      unused(compiler);
      return std::visit([&](auto && o) { return evaluate_literal_symbols(meta, id, o, ctx); }, decl.value);
    }

    bool evaluate_statement_symbols(context * compiler, program_metadata * meta, size_t id,
                                    expr::call_parameter const & param, symbol_eval_context const & ctx) {
      unused(id);
      return evaluate_symbols(compiler, meta, param.expression, ctx) &&
             (!param.next.has_value() || evaluate_symbols(compiler, meta, param.next.value(), ctx));
    }

    bool evaluate_statement_symbols(context * compiler, program_metadata * meta, size_t id, expr::binary_operator const & op,
                                    symbol_eval_context const & ctx) {
      unused(id);
      switch (op.type_name) {
      case expr::operator_type::call: {
        assert(op.left.has_value());
        if (op.right.has_value() && !evaluate_symbols(compiler, meta, op.right.value(), ctx))
          return false;

        symbol_eval_context callCtx = ctx;
        callCtx.is_call             = true;
        callCtx.call_parameter_list = op.right;
        if (!evaluate_symbols(compiler, meta, op.left.value(), callCtx)) {
          return false;
        }

        meta->statement_info[id].type_ref = meta->statement_info[op.left.value()].type_ref->return_type();
        break;
      }
      default: {
        assert(op.left.has_value());
        assert(op.right.has_value());
        if (!(evaluate_symbols(compiler, meta, op.left.value(), ctx) &&
              evaluate_symbols(compiler, meta, op.right.value(), ctx))) {
          printf("Error: failed to evaluate symbols for binary operator operands\n");
          return false;
        }

        const auto lhsType = meta->statement_info[op.left.value()].type_ref;
        const auto rhsType = meta->statement_info[op.right.value()].type_ref;
        if (!(lhsType.has_value() && rhsType.has_value())) {
          printf("Error: type of binary operator operands could not be determined\n");
          return false;
        }
        const auto symbol = meta->search_for_operator_symbol(ctx.scope_id, op.type_name, lhsType.value(), rhsType.value());
        if (!symbol.has_value()) {
          const auto opName  = expr::get_operator_identifer(op.type_name);
          const auto lhsName = lhsType->get_identifier();
          const auto rhsName = rhsType->get_identifier();

          printf("Error: no suitable binary operator: op='%.*s', lhs='%.*s', rhs='%.*s'\n", (int)opName.length(),
                 opName.data(), (int)lhsName.length(), lhsName.data(), (int)rhsName.length(), rhsName.data());

          return false;
        }

        auto it = std::find(meta->symbol_references.begin(), meta->symbol_references.end(), symbol.value());
        meta->statement_info[id].symbol_index = it - meta->symbol_references.begin();
        meta->statement_info[id].type_ref     = symbol->get_type().return_type();
        break;
      }
      }
      return true;
    }

    bool evaluate_statement_symbols(context * compiler, program_metadata * meta, size_t id, expr::block const & block,
                                    symbol_eval_context const & ctx) {
      auto & statementInfo = meta->statement_info[id];
      // size_t thisBlockScopeId = 0;
      symbol_eval_context thisBlockCtx = ctx;
      if (statementInfo.scope_index.has_value()) {
        // Already have a scope index.
        // This is the root scope of a function declaration.
        thisBlockCtx.scope_id = statementInfo.scope_index.value();
      } else {
        thisBlockCtx.scope_id = meta->new_scope(ctx.scope_id);

        statementInfo.scope_index = thisBlockCtx.scope_id;

        program_metadata::scope & newScope = meta->scopes[thisBlockCtx.scope_id];
        newScope.parent_function_scope     = ctx.function_root_scope_id;
        newScope.prefix = adder::format("%s%s/", meta->scopes[ctx.scope_id].prefix.c_str(), block.scope_name.c_str());
      }

      for (auto & statement : block.statements) {
        if (!evaluate_symbols(compiler, meta, statement, thisBlockCtx)) {
          return false;
        }
      }

      return true;
    }

    bool evaluate_statement_symbols(context * compiler, program_metadata * meta, size_t id,
                                    expr::function_declaration const & decl, symbol_eval_context const & ctx) {
      symbol symbol;
      symbol.name           = decl.identifier;
      symbol.scope_id       = ctx.scope_id;
      symbol.flags          = decl.flags | symbol_flags::function;
      symbol.declaration_id = id;
      if (decl.type.has_value()) {
        const auto typeIndex = evaluate_type_index(meta, decl.type.value());
        if (!typeIndex.has_value()) {
          // TODO: Error. Expected a type name
          return false;
        }
        symbol.type = typeIndex.value();
      } else {
        // TODO: Function type is unknown
        return false;
      }

      const std::string name = std::string(decl.identifier);
      symbol.full_identifier = name;
      symbol.full_identifier = adder::format("%s%s", meta->scopes[ctx.scope_id].prefix.c_str(),
                                             get_symbol_name(symbol.type.get_identifier(), name).c_str());

      const auto symbolRef = meta->add_symbol(symbol);
      if (!symbolRef.has_value()) {
        // TODO: Throw error. Probably need to get the failure reason from add_symbol.
        return false;
      }

      meta->statement_info[id].symbol_index = meta->symbol_references.size() - 1;

      if (decl.body.has_value()) {
        symbol_eval_context thisBlockCtx    = ctx;
        thisBlockCtx.scope_id               = meta->new_scope(ctx.scope_id);
        thisBlockCtx.function_root_scope_id = thisBlockCtx.scope_id;

        meta->statement_info[id].scope_index                = thisBlockCtx.scope_id;
        meta->statement_info[decl.body.value()].scope_index = thisBlockCtx.scope_id;

        {
          program_metadata::scope & functionScope = meta->scopes[thisBlockCtx.scope_id];
          functionScope.parent_function_scope     = std::nullopt;
          functionScope.prefix = adder::format("%.*s/", symbol.full_identifier.length(), symbol.full_identifier.data());
        }

        meta->get(symbolRef->index).function_root_scope_id = thisBlockCtx.scope_id;

        for (const size_t statement : decl.arguments) {
          // TODO: Should arguments be part of the "block" statement?
          if (!evaluate_symbols(compiler, meta, statement, thisBlockCtx)) {
            return false;
          }
        }

        evaluate_symbols(compiler, meta, decl.body.value(), thisBlockCtx);
      }

      return true;
    }

    bool evaluate_statement_symbols(context * compiler, program_metadata * meta, size_t id,
                                    expr::variable_declaration const & decl, symbol_eval_context const & ctx) {
      symbol symbol;
      symbol.name           = decl.name;
      symbol.scope_id       = ctx.scope_id;
      symbol.declaration_id = id;
      symbol.flags          = decl.flags;

      if (decl.type.has_value()) {
        const auto typeIndex = evaluate_type_index(meta, decl.type.value());
        if (!typeIndex.has_value()) {
          // TODO: Error. Expected a type name
          return false;
        }
        symbol.type = typeIndex.value();
      } else if (decl.initializer.has_value()) {
        symbol.type = eval_decltype(meta, decl.initializer.value());
      } else {
        // TODO: Error. Unable to infer type. No initializer statement.
        return false;
      }

      if (symbol.type == meta->get_type_reference(type_primitive::void_)) {
        // TODO: Error. Symbol does not have a valid type.
        return false;
      }

      symbol.full_identifier = adder::format("%s%s", meta->scopes[ctx.scope_id].prefix.c_str(),
                                             get_symbol_name(symbol.type.get_identifier(), symbol.name).c_str());

      auto symbolRef = meta->add_symbol(symbol);
      if (!symbolRef.has_value()) {
        // TODO: Throw error. Probably need to get the failure reason from add_symbol.
        return false;
      }
      auto symbolIndex = meta->symbol_references.size() - 1;

      if (decl.initializer.has_value()) {
        evaluate_symbols(compiler, meta, decl.initializer.value(), ctx);
      }

      meta->statement_info[id].symbol_index = symbolIndex;
      meta->statement_info[id].type_ref     = symbol.type;
      return true;
    }

    bool evaluate_statement_symbols(context * compiler, program_metadata * meta, size_t id,
                                    expr::import_symbol const & import_stmt, symbol_eval_context const & ctx) {
      unused(ctx);
      std::string                       name                 = (std::string)import_stmt.module_name;
      std::shared_ptr<program_metadata> imported_module_meta = compiler->get_module_metadata(meta->module_name, name);

      imported_module_meta->search_for_symbol(0, [&](const symbol_reference & ref) {
        if (ref.get().name != import_stmt.symbol_name) {
          return false;
        }

        auto symbol = meta->add_symbol(ref);
        if (!symbol.has_value())
          return false;
        auto symbolIndex = meta->symbol_references.size() - 1;
        if (!meta->statement_info[id].symbol_index.has_value())
          meta->statement_info[id].symbol_index = meta->symbol_references.size() - 1;
        else
          meta->statement_info[id].symbol_count = 1 + symbolIndex - meta->statement_info[id].symbol_index.value();

        return false;
      });

      return true;
    }

    bool evaluate_statement_symbols(context * compiler, program_metadata * meta, size_t id,
                                    expr::import_module const & import_stmt, symbol_eval_context const & ctx) {
      unused(ctx);

      std::string                       name                 = (std::string)import_stmt.module_name;
      std::shared_ptr<program_metadata> imported_module_meta = compiler->get_module_metadata(meta->module_name, name);

      imported_module_meta->search_for_symbol(0, [&](const symbol_reference & ref) {
        // TODO: Explicit exports
        // if (/* is private, or is not exported */) {
        //   return false;
        // }

        auto symbol = meta->add_symbol(ref);
        if (!symbol.has_value())
          return false;

        auto symbolIndex = meta->symbol_references.size() - 1;

        if (!meta->statement_info[id].symbol_index.has_value())
          meta->statement_info[id].symbol_index = symbolIndex;
        else
          meta->statement_info[id].symbol_count = 1 + symbolIndex - meta->statement_info[id].symbol_index.value();

        return false;
      });

      for (size_t i = 0; i < imported_module_meta->types->size(); ++i) {
        meta->add_type({imported_module_meta.get(), type_id{i}});
      }

      return true;
    }

    bool evaluate_symbols(context * compiler, program_metadata * meta, size_t id, symbol_eval_context const & ctx) {
      return std::visit([&](auto && s) { return evaluate_statement_symbols(compiler, meta, id, s, ctx); },
                        meta->tree.statements[id]);
    }

    bool evaluate_symbols(context * compiler, program_metadata * meta) {
      meta->symbols.clear();
      meta->scopes.clear();
      meta->scopes.emplace_back(); // Global scope.

      meta->statement_info.clear();
      meta->statement_info.resize(meta->tree.statements.size());

      expr::block const & top = meta->tree.get<expr::block>(meta->tree.statements.size() - 1);
      symbol_eval_context ctx;
      ctx.scope_id = 0;
      for (size_t statementId : top.statements) {
        if (!evaluate_symbols(compiler, meta, statementId, ctx)) {
          return false;
        }
      }
      return true;
    }
    
    /// Evaluate the type index of a ast statement that refers to a type
    std::optional<type_reference> evaluate_type_index(program_metadata * meta, size_t statementId) {
      std::optional<size_t> ret;
      auto& typeId = meta->statement_info[statementId].type_ref;
      if (typeId.has_value()) {
        return typeId;
      }

      if (meta->tree.is<expr::type_modifier>(statementId)) {
        expr::type_modifier const & modifier = meta->tree.get<expr::type_modifier>(statementId);
        auto modified = evaluate_type_index(meta, modifier.modified);
        if (!modified.has_value()) {
          printf("Error: reference to unknown type\n");
          return std::nullopt;
        }
        type_modifier mod;
        mod.base = modified.value();
        mod.const_ = modifier.const_;
        mod.reference = modifier.reference;

        type t;
        t.desc = mod;
        t.identifier = get_type_name(meta->tree, statementId).value();
        typeId = meta->add_type(t);
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
        t.desc = desc;
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
  } // namespace compiler
} // namespace adder