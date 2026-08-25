#include "compiler/program_metadata.h"
#include "compiler/ast.h"

namespace adder {
  namespace compiler {
    symbol const & symbol_reference::get() const {
      return meta->get(index);
    }

    size_t symbol_reference::get_size() const {
      return get_type().get_size();
    }

    type_reference symbol_reference::get_type() const {
      return get().type;
    }

    std::optional<type_reference> program_metadata::get_type_reference(type_primitive const & primitive) const {
      return get_type_reference(get_primitive_type_name(primitive));
    }

    std::optional<type_reference> program_metadata::get_type_reference(std::string_view const & name) const {
      const auto it = std::find_if(type_references.begin(), type_references.end(),
                                   [&](type_reference const & t) { return t.get_identifier()  == name; });

      if (it == type_references.end())
        return std::nullopt;

      return *it;
    }

    type const * program_metadata::get_type(std::string_view const & name) const {
      const auto it = std::find_if(type_references.begin(), type_references.end(),
                                   [&](type_reference const & t) { return t.get_identifier() == name; });
      if (it == type_references.end())
        return nullptr;

      return &it->get_type();
    }

    std::optional<type_reference> program_metadata::get_type_reference(size_t type) const {
      const auto name = get_type_name(tree, type);
      return name.has_value() ? get_type_reference(name.value()) : std::nullopt;
    }

    type const * program_metadata::get_type(size_t type) const {
      const auto name = get_type_name(tree, type);
      return name.has_value() ? get_type(name.value()) : nullptr;
    }

    size_t program_metadata::get_type_size(type_modifier const & desc) const {
      return desc.reference ? sizeof(vm::address_t) : desc.base.get_size();
    }

    size_t program_metadata::get_type_size(type_primitive const & desc) const {
      switch (desc) {
      case type_primitive::void_: return 0;
      case type_primitive::int8: return sizeof(int8_t);
      case type_primitive::int16: return sizeof(int16_t);
      case type_primitive::int32: return sizeof(int32_t);
      case type_primitive::int64: return sizeof(int64_t);
      case type_primitive::uint8: return sizeof(uint8_t);
      case type_primitive::uint16: return sizeof(uint16_t);
      case type_primitive::uint32: return sizeof(uint32_t);
      case type_primitive::uint64: return sizeof(uint64_t);
      case type_primitive::float32: return sizeof(float);
      case type_primitive::float64: return sizeof(double);
      case type_primitive::bool_: return sizeof(bool);
      }
      return 0;
    }

    size_t program_metadata::get_type_size(type_class const & desc) const {
      return desc.size;
    }

    size_t program_metadata::get_type_size(type_function const & desc) const {
      return desc.size;
    }

    size_t program_metadata::get_type_size(type_function_decl const & desc) const {
      return desc.size;
    }

    size_t program_metadata::get_type_size(type_id const & typeIndex) const {
      return get_type_size(get(typeIndex));
    }

    size_t program_metadata::get_type_size(type const & type) const {
      return std::visit([this](auto const & o) { return get_type_size(o); }, type.desc);
    }

    size_t program_metadata::new_scope(size_t parent) {
      const size_t newScopeId = scopes.size();
      scopes.emplace_back();

      scope & newScope = scopes.back();
      newScope.parent  = parent;

      scope & parentScope = scopes[parent];
      if (!parentScope.first_child.has_value()) {
        parentScope.first_child = newScopeId;
        return newScopeId;
      }

      size_t lastSibling = parentScope.first_child.value();
      for (; scopes[lastSibling].sibling.has_value(); lastSibling = scopes[lastSibling].sibling.value())
        ;
      scopes[lastSibling].sibling = newScopeId;

      return newScopeId;
    }

    void program_metadata::for_each_child_scope(size_t rootId, std::function<void(size_t)> const & cb) {
      scope & root = scopes[rootId];
      if (!root.first_child.has_value()) {
        return;
      }

      size_t next = root.first_child.value();
      cb(next);
      for (; scopes[next].sibling.has_value(); next = scopes[next].sibling.value())
        cb(scopes[next].sibling.value());
    }

    type_reference program_metadata::add_type(type_reference const & desc) {
      const auto existing = get_type_reference(desc.get_identifier());
      if (existing.has_value())
        return existing.value();

      type_references.push_back(desc);
      return {this, type_id{types.size() - 1}};
    }

    type_reference program_metadata::add_type(type const & desc) {
      const auto existing = get_type_reference(desc.identifier);
      if (existing.has_value())
        return existing.value();

      types.push_back(desc);
      type_references.push_back(type_reference{this, type_id{types.size() - 1}});
      return type_references.back();
    }

    type_reference program_metadata::add_function_type(expr::function_declaration const & decl,
                                               std::optional<size_t> id) {
      type_function_decl fn;
      fn.allowInline;
      fn.function_id = id.value();
      fn.type        = get_type_reference(decl.type.value()).value_or(type_reference::undefined());

      if (is_undefined(fn.type)) {
        // TODO: Log error. Invalid function type.
        return type_reference::undefined();
      }

      type t;
      t.identifier = decl.identifier.empty() ? adder::format("__unnamed_fn_%lld", id.value()) : decl.identifier;
      t.identifier = adder::format("%s%.*s", t.identifier.c_str(), fn.type.get_identifier().length(), fn.type.get_identifier().data());
      t.desc       = fn;

      return add_type(t);
    }

    
    std::optional<symbol_reference> program_metadata::add_symbol(symbol_reference const & ref) {
      auto & s = ref.get();
      if (s.scope_id >= scopes.size() || s.scope_id != 0)
        return std::nullopt;

      for (const symbol_reference &existing : scopes[s.scope_id].symbols) {
        if (existing.get().name != s.name) {
          continue;
        }

        if (existing.get_type() == s.type) {
          return std::nullopt; // Duplicate symbol
        }

        // TODO: Test if s.type can overload the existing symbol
        if (!is_valid_function_overload(existing.get_type(), s.type)) {
          return std::nullopt;
        }
      }

      symbol_references.push_back(ref);
      scopes[s.scope_id].symbols.push_back(ref);

      return ref;
    }

    std::optional<symbol_reference> program_metadata::add_symbol(symbol const & s) {
      if (s.scope_id >= scopes.size())
        return std::nullopt;

      for (const symbol_reference &existing : scopes[s.scope_id].symbols) {
        if (existing.get().name != s.name) {
          continue;
        }

        if (existing.get_type() == s.type) {
          return std::nullopt; // Duplicate symbol
        }

        // TODO: Test if s.type can overload the existing symbol
        if (!is_valid_function_overload(existing.get_type(), s.type)) {
          return std::nullopt;
        }
      }

      const size_t symbolIndex = symbols.size();
      symbols.push_back(s);
      symbol_references.push_back({this, symbol_id{symbolIndex}});
      scopes[s.scope_id].symbols.push_back({ this, symbol_id{symbolIndex} });

      return scopes[s.scope_id].symbols.back();
    }

    std::optional<symbol_reference> program_metadata::get_statement_symbol(size_t statementId, size_t idx) const {
      std::optional<size_t> base = statement_info[statementId].symbol_index;
      if (!base.has_value())
        return std::nullopt;
      return symbol_references[base.value() + idx];
    }

    size_t program_metadata::get_statement_symbol_count(size_t statementId) const {
      if (!statement_info[statementId].symbol_index.has_value()) {
        return 0;
      }
      return statement_info[statementId].symbol_count;
    }

    std::optional<symbol_reference> program_metadata::search_for_symbol(size_t                   scopeId,
                                                                        std::string_view const & name) const {
      return search_for_symbol(scopeId, [&name](symbol_reference const & s) { return s.get().name == name; });
    }

    std::optional<symbol_reference>
    program_metadata::search_for_symbol(size_t                                              scopeId,
                                              std::function<bool(symbol_reference const &)> const & pred) const {
      auto found = std::find_if(scopes[scopeId].symbols.rbegin(), scopes[scopeId].symbols.rend(),
                                [&](symbol_reference const & ref) { return pred(ref); });
      if (found != scopes[scopeId].symbols.rend()) {
        return *found;
      }

      if (!scopes[scopeId].parent.has_value())
        return std::nullopt;

      return search_for_symbol(scopes[scopeId].parent.value(), pred);
    }

    std::optional<symbol_reference> program_metadata::search_for_callable_symbol(size_t                        scopeId,
                                                                             std::string_view const &      identifier,
                                                                             std::optional<size_t> const & paramList) const {
      std::optional<symbol_reference> bestFunction;
      std::optional<size_t> bestMatchScore;
      bool                  ambigious = false;

      search_for_symbol(scopeId, [&](symbol_reference const & sym) {
        if (sym.get().name != identifier) {
          return false;
        }
        auto value = get_parameter_list_score(scopeId, sym.get_type(), paramList);
        if (!value.has_value())
          return false;
        if (bestMatchScore.has_value() && value->value > bestMatchScore.value())
          return false;
        if (value->value == bestMatchScore) {
          ambigious = true;
          bestFunction.reset();
          return false;
        }
        bestMatchScore = value->value;
        bestFunction   = sym;
        return false;
      });

      if (ambigious)
        printf("Error: Ambigous call to '%.*s'\n", (int)identifier.length(), identifier.data());

      return bestFunction;
    }

    std::optional<symbol_reference> program_metadata::search_for_operator_symbol(size_t scopeId, expr::operator_type op,
                                                                             type_reference lhsType,
                                                                             type_reference rhsType) const {
      std::string_view      identifier = expr::get_operator_identifer(op);
      std::optional<symbol_reference> bestFunction;
      std::optional<parameter_list_score> bestMatchScore;
      bool                  ambigious = false;
      search_for_symbol(scopeId, [&](symbol_reference const & sym) {
        if (sym.get().name != identifier)
          return false;
        if (get_functor_type(sym.get_type()) != functor_type::operator_)
          return false;

        type_reference const types[2] = {lhsType, rhsType};
        auto                 value    = get_parameter_list_score(scopeId, sym.get_type(), types, 2);

        if (!value.has_value())
          return false;

        if (bestMatchScore.has_value() && bestMatchScore.value() < value)
          return false;

        if (value == bestMatchScore) {
          ambigious = true;
          bestFunction.reset();
          return false;
        }

        bestMatchScore = value;
        bestFunction   = sym;
        ambigious      = false;
        return false;
      });

      if (ambigious)
        printf("Error: Ambigous call to '%.*s'\n", (int)identifier.length(), identifier.data());

      return bestFunction;
    }

    namespace {
      // Implements the parameter list scoring algorithm
      struct parameter_list_score_calculator {
        type_function const * signature = nullptr;
        parameter_list_score  value; // Lower is better
        size_t                scope_id = 0;

        size_t i = 0;

        bool next(program_metadata const * meta, type_reference param) {
          if (complete())
            return false;

          type_reference const & arg = signature->arguments[i++];
          if (param != arg) {
            if (arg.is_reference_of(param))
              return true;

            auto initializer = meta->find_unnamed_initializer(scope_id, arg, param);
            if (!initializer.has_value())
              return false; // No conversion available
            if (!value.first_conversion_index.has_value())
              value.first_conversion_index = i - 1;
            ++value.value;
          }

          return true;
        }

        bool complete() const {
          return i == signature->arguments.size();
        }
      };
    } // namespace

    std::optional<parameter_list_score>
    program_metadata::get_parameter_list_score(size_t scopeId, type_reference funcType,
                                               std::optional<size_t> const & paramList) const {
      if (!is_function(funcType)) {
        return std::nullopt;
      }
      auto decayed = remove_reference(decay_type(funcType));
      if (!decayed.has_value()) {
        return std::nullopt;
      }

      parameter_list_score_calculator scoreCalculator;
      scoreCalculator.signature = &decayed->as<type_function>();
      scoreCalculator.scope_id  = scopeId;

      std::optional<size_t> current = paramList;
      while (current.has_value()) {
        auto & param = tree.get<expr::call_parameter>(current.value());
        if (!scoreCalculator.next(this, statement_info[param.expression].type_ref.value())) {
          return std::nullopt; // Too many arguments
        }
        current = param.next;
      }

      if (!scoreCalculator.complete()) {
        return std::nullopt; // Not enough arguments
      }

      return scoreCalculator.value;
    }

    std::optional<parameter_list_score> program_metadata::get_parameter_list_score(size_t scopeId, type_reference funcType,
                                                                                   type_reference const * paramList,
                                                                                   size_t         numParams) const {
      if (!is_function(funcType)) {
        return std::nullopt;
      }
      auto decayed = decay_type(funcType);
      if (!decayed.has_value()) {
        return std::nullopt;
      }

      parameter_list_score_calculator scoreCalculator;
      scoreCalculator.signature = &decayed->as<type_function>();
      scoreCalculator.scope_id  = scopeId;

      for (size_t i = 0; i < numParams; ++i)
        if (!scoreCalculator.next(this, paramList[i]))
          return std::nullopt; // Too many arguments

      if (!scoreCalculator.complete())
        return std::nullopt; // Not enough arguments

      return scoreCalculator.value;
    }

    std::optional<symbol_reference> program_metadata::find_symbol(std::string_view const & fullName) const {
      auto found =
        std::find_if(symbols.begin(), symbols.end(), [&](symbol const & s) { return s.full_identifier == fullName; });
      if (found == symbols.end())
        return std::nullopt;

      return symbol_reference{ this, symbol_id{size_t(found - symbols.begin())} };
    }

    std::optional<symbol_reference> program_metadata::find_unnamed_initializer(size_t scopeId, type_reference receiverTypeIndex,
                                                                     type_reference initializerTypeIndex) const {
      std::string_view fullName = adder::format("init ([ref]%.*s,%.*s)=>void:", receiverTypeIndex.get_identifier().length(),
                                                receiverTypeIndex.get_identifier().data(),
                                                initializerTypeIndex.get_identifier().length(),
                                                initializerTypeIndex.get_identifier().data());

      return search_for_symbol(scopeId, [fullName](symbol_reference const & sym) { return sym.get().full_identifier == fullName; });
    }

    std::optional<size_t> program_metadata::get_parent_scope(size_t const & scopeId) const {
      if (scopeId < scopes.size())
        return scopes[scopeId].parent;
      else
        return std::nullopt;
    }

    std::optional<std::string> get_symbol_name(ast const & ast, size_t statement, std::string_view const & identifier) {
      auto typeName = get_type_name(ast, statement);
      if (!typeName.has_value())
        return std::nullopt;

      return get_symbol_name(typeName.value(), identifier);
    }

    std::string get_symbol_name(std::string_view const & typeName, std::string_view const & identifier) {
      return (std::string)adder::format("%.*s:%.*s", typeName.length(), typeName.data(), identifier.length(),
                                        identifier.data());
    }

    std::optional<std::string> get_type_name(ast const & ast, size_t statement) {
      if (ast.is<expr::type_name>(statement)) {
        return std::string(ast.get<expr::type_name>(statement).name);
      }

      if (ast.is<expr::type_modifier>(statement)) {
        auto & modifier = ast.get<expr::type_modifier>(statement);
        if (!(modifier.const_ || modifier.reference))
          return get_type_name(ast, modifier.modified);
        std::string ret = "[";
        if (modifier.const_)
          ret += "const";
        if (modifier.reference)
          ret += "ref";
        ret += "]";
        auto nested = get_type_name(ast, modifier.modified);
        if (!nested.has_value())
          return std::nullopt;
        return ret + nested.value();
      }

      if (ast.is<expr::type_fn>(statement)) {
        auto &      fn  = ast.get<expr::type_fn>(statement);
        std::string ret = "(";
        for (size_t i = 0; i < fn.argument_list.size(); ++i) {
          auto arg = get_type_name(ast, fn.argument_list[i]);
          if (!arg.has_value())
            return std::nullopt;
          ret += arg.value();
          if (i != fn.argument_list.size() - 1)
            ret += ",";
        }

        auto returnName = get_type_name(ast, fn.return_type);
        if (!returnName.has_value())
          return std::nullopt;

        switch (fn.func_type) {
        case functor_type::free: return ret + ")=>" + returnName.value();
        case functor_type::member: return "mem " + ret + ")=>" + returnName.value();
        case functor_type::initializer: return "init " + ret + ")=>" + returnName.value();
        case functor_type::operator_: return "op " + ret + ")=>" + returnName.value();
        }
      }

      return std::nullopt;
    }
  }
}
