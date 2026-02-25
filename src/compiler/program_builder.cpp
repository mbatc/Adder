#include "program_builder.h"
#include "../compiler.h"
#include "../program.h"

namespace adder {
  namespace compiler {
    std::optional<size_t> program_metadata::get_type_index(std::string_view const& name) const {
      const auto it = std::find_if(types.begin(), types.end(), [&](type const& t) { return t.identifier == name; });

      if (it == types.end())
        return std::nullopt;

      return it - types.begin();
    }

    type const * program_metadata::get_type(std::string_view const& name) const {
      const auto it = std::find_if(types.begin(), types.end(), [&](type const& t) { return t.identifier == name; });
      if (it == types.end())
        return nullptr;
      return &(*it);
    }

    std::optional<size_t> program_metadata::get_type_index(ast const & tree, size_t type) const {
      const auto name = get_type_name(tree, type);
      return name.has_value() ? get_type_index(name.value()) : std::nullopt;
    }

    type const * program_metadata::get_type(ast const & tree, size_t type) const {
      const auto name = get_type_name(tree, type);
      return name.has_value() ? get_type(name.value()) : nullptr;
    }

    std::optional<size_t> program_metadata::unwrap_type(std::optional<size_t> const & type) const {
      if (type.has_value() && std::holds_alternative<type_modifier>(types[type.value()].desc))
        return std::get<type_modifier>(types[type.value()].desc).base;
      else
        return std::nullopt;
    }

    std::optional<size_t> program_metadata::decay_type(std::optional<size_t> const & type) const {
      if (!type.has_value())
        return std::nullopt;

      const auto& desc = types[type.value()].desc;
      if (std::holds_alternative<type_modifier>(desc)) {
        const auto &modifier = std::get<type_modifier>(desc);
        if (modifier.const_) {
          return decay_type(modifier.base);
        }
      }
      else if (std::holds_alternative<type_function_decl>(desc)) {
        const auto &decl = std::get<type_function_decl>(desc);
        return decl.type;
      }

      return type;
    }

    std::optional<size_t> program_metadata::return_type_of(std::optional<size_t> const& func) const {
      if (!func.has_value())
        return std::nullopt;

      if (std::holds_alternative<type_function>(types[func.value()].desc)) {
        return std::get<type_function>(types[func.value()].desc).return_type;
      }

      if (std::holds_alternative<type_function_decl>(types[func.value()].desc)) {
        return return_type_of(std::get<type_function_decl>(types[func.value()].desc).type);
      }

      return return_type_of(unwrap_type(func.value()));
    }

    bool program_metadata::is_reference_of(std::optional<size_t> const & reference, std::optional<size_t> const & baseType) const {
      if (!(reference.has_value()
        && baseType.has_value()
        && std::holds_alternative<type_modifier>(types[*reference].desc)))
        return false;

      auto& modifiers = std::get<type_modifier>(types[*reference].desc);
      return modifiers.reference && modifiers.base == baseType;
    }

    bool program_metadata::is_reference(std::optional<size_t> const & type) const
    {
      return type.has_value()
        && std::holds_alternative<type_modifier>(types[*type].desc)
        && std::get<type_modifier>(types[*type].desc).reference;
    }

    bool program_metadata::is_function_decl(std::optional<size_t> const& type) const {
      return type.has_value() && std::holds_alternative<type_function_decl>(types[*type].desc);
    }

    bool program_metadata::is_function(std::optional<size_t> const& type) const {
      return type.has_value()
        && (std::holds_alternative<type_function_decl>(types[*type].desc)
          || std::holds_alternative<type_function>(types[*type].desc)
          || is_function(unwrap_type(*type)));
    }

    functor_type program_metadata::get_functor_type(std::optional<size_t> const& type) const {
      auto decayed = decay_type(type);
      if (!(decayed.has_value() && std::holds_alternative<type_function>(types[*decayed].desc)))
        return functor_type::none;
      auto &decl = std::get<type_function>(types[*decayed].desc);
      return decl.func_type;
    }

    bool program_metadata::is_const(std::optional<size_t> const & type) const {
      return type.has_value()
        && std::holds_alternative<type_modifier>(types[*type].desc)
        && std::get<type_modifier>(types[*type].desc).const_;
    }

    bool program_metadata::is_integer(std::optional<size_t> const & type) const {
      return type.has_value()
        && std::holds_alternative<type_primitive>(types[*type].desc)
        && compiler::is_integer(std::get<type_primitive>(types[*type].desc));
    }

    bool program_metadata::is_float(std::optional<size_t> const & type) const {
      return type.has_value()
        && std::holds_alternative<type_primitive>(types[*type].desc)
        && compiler::is_float(std::get<type_primitive>(types[*type].desc));
    }

    bool program_metadata::is_bool(std::optional<size_t> const & type) const {
      return type.has_value()
        && std::holds_alternative<type_primitive>(types[*type].desc)
        && compiler::is_bool(std::get<type_primitive>(types[*type].desc));
    }

    bool program_metadata::is_void(std::optional<size_t> const& type) const {
      return type.has_value()
        && std::holds_alternative<type_primitive>(types[*type].desc)
        && compiler::is_void(std::get<type_primitive>(types[*type].desc));
    }

    bool program_metadata::is_valid_function_overload(std::optional<size_t> const & a, std::optional<size_t> const & b) const {
      if (!a.has_value() || !b.has_value()) {
        return false;
      }

      if (!is_function(a) || !is_function(b)) {
        return false;
      }

      auto decayedA = decay_type(a);
      auto decayedB = decay_type(b);
      if (!decayedA.has_value() || !decayedB.has_value()) {
        return false;
      }

      return std::get<type_function>(types[decayedA.value()].desc).arguments != std::get<type_function>(types[decayedB.value()].desc).arguments;
    }

    size_t program_metadata::get_type_size(type_modifier const & desc) const {
      return desc.reference ? sizeof(vm::address_t) : get_type_size(desc.base);
    }

    size_t program_metadata::get_type_size(type_primitive const& desc) const {
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

    size_t program_metadata::get_type_size(type_class const& desc) const {
      return desc.size;
    }

    size_t program_metadata::get_type_size(type_function const& desc) const {
      return desc.size;
    }

    size_t program_metadata::get_type_size(type_function_decl const& desc) const {
      return desc.size;
    }

    size_t program_metadata::get_type_size(size_t const & typeIndex) const {
      return get_type_size(types[typeIndex]);
    }

    size_t program_metadata::get_type_size(type const& type) const {
      return std::visit([this](auto const& o) {
        return get_type_size(o);
        }, type.desc);
    }

    size_t program_metadata::new_scope(size_t parent) {
      const size_t newScopeId = scopes.size();
      scopes.emplace_back();

      scope & newScope = scopes.back();
      newScope.parent = parent;

      scope & parentScope = scopes[parent];
      if (!parentScope.first_child.has_value()) {
        parentScope.first_child = newScopeId;
        return newScopeId;
      }

      size_t lastSibling = parentScope.first_child.value();
      for (; scopes[lastSibling].sibling.has_value(); lastSibling = scopes[lastSibling].sibling.value());
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

    size_t program_metadata::add_type(type const & desc) {
      const auto existing = get_type_index(desc.identifier);
      if (existing.has_value())
        return existing.value();

      types.push_back(desc);
      return types.size() - 1;
    }

    size_t program_metadata::add_function_type(ast const& tree, expr::function_declaration const& decl, std::optional<size_t> id) {
      type_function_decl fn;
      fn.allowInline;
      fn.function_id = id.value();
      fn.type = get_type_index(tree, decl.type.value()).value_or(0);

      if (fn.type == 0) {
        // TODO: Log error. Invalid funciton type.
        return 0;
      }

      type t;
      t.identifier = decl.identifier.empty() ? adder::format("__unnamed_fn_%lld", id.value()) : decl.identifier;
      t.identifier = adder::format("%s%s", t.identifier.c_str(), types[fn.type].identifier.c_str());
      t.desc = fn;

      return add_type(t);
    }

    std::optional<size_t> program_metadata::add_symbol(symbol const & s) {
      if (s.scope_id >= scopes.size())
        return std::nullopt;

      for (const size_t existingId : scopes[s.scope_id].symbols) {
        const auto& existing = symbols[existingId];
        if (existing.name != s.name) {
          continue;
        }

        if (existing.type == s.type) {
          return std::nullopt; // Duplicate symbol
        }

        // TODO: Test if s.type can overload the existing symbol
        if (!is_valid_function_overload(existing.type, s.type)) {
          return std::nullopt;
        }
      }

      const size_t symbolIndex = symbols.size();
      symbols.push_back(s);
      scopes[s.scope_id].symbols.push_back(symbolIndex);

      return symbolIndex;
    }

    size_t program_metadata::get_symbol_size(size_t const& symbolIndex) const {
      return get_type_size(get_symbol_type(symbolIndex));
    }

    size_t program_metadata::get_symbol_type(size_t const& symbolIndex) const {
      return symbols[symbolIndex].type;
    }

    std::optional<size_t> program_metadata::search_for_symbol_index(size_t scopeId, std::string_view const & name) const {
      return search_for_symbol_index(scopeId, [&name](symbol const& s) {
        return s.name == name;
      });
    }

    std::optional<size_t> program_metadata::search_for_symbol_index(size_t scopeId, std::function<bool(symbol const &)> const & pred) const {
      return search_for_symbol_index(scopeId, [&pred](symbol const& sym, size_t) { return pred(sym); });
    }

    std::optional<size_t> program_metadata::search_for_symbol_index(size_t scopeId, std::function<bool(symbol const&, size_t)> const& pred) const {
      auto found = std::find_if(scopes[scopeId].symbols.rbegin(), scopes[scopeId].symbols.rend(), [&](int64_t idx) { return pred(symbols[idx], idx); });
      if (found != scopes[scopeId].symbols.rend()) {
        return *found;
      }

      if (!scopes[scopeId].parent.has_value())
        return std::nullopt;

      return search_for_symbol_index(scopes[scopeId].parent.value(), pred);
    }

    std::optional<size_t> program_metadata::search_for_callable_symbol_index(size_t scopeId, std::string_view const& identifier, ast const& ast, std::optional<size_t> const & paramList) const {
      std::optional<size_t> bestFunction;
      std::optional<size_t> bestMatchScore;
      bool ambigious = false;

      search_for_symbol_index(scopeId, [&](symbol const& sym, size_t idx) {
        if (sym.name != identifier) {
          return false;
        }
        auto score = get_parameter_list_score(scopeId, sym.type, ast, paramList);
        if (!score.has_value())
          return false;
        if (bestMatchScore.has_value() && score.value() > bestMatchScore.value())
          return false;
        if (score == bestMatchScore) {
          ambigious = true;
          bestFunction.reset();
          return false;
        }
        bestMatchScore = score;
        bestFunction = idx;
        return false;
      });

      if (ambigious)
        printf("Error: Ambigous call to '%.*s'\n", (int)identifier.length(), identifier.data());

      return bestFunction;
    }

    std::optional<size_t> program_metadata::search_for_operator_symbol_index(size_t scopeId, expr::operator_type op, size_t lhsType, size_t rhsType) const {
      std::string_view identifier = expr::get_operator_identifer(op);
      std::optional<size_t> bestFunction;
      std::optional<size_t> bestMatchScore;
      bool ambigious = false;
      search_for_symbol_index(scopeId, [&](symbol const & sym, size_t idx) {
        if (sym.name != identifier)
          return false;
        if (get_functor_type(sym.type) != functor_type::operator_)
          return false;
        size_t const types[2] = { lhsType, rhsType };
        auto score = get_parameter_list_score(scopeId, sym.type, types, 2);

        if (!score.has_value())
          return false;

        if (bestMatchScore.has_value() && score.value() > bestMatchScore.value())
          return false;

        if (score == bestMatchScore) {
          ambigious = true;
          bestFunction.reset();
          return false;
        }

        bestMatchScore = score;
        bestFunction = idx;
        ambigious = false;
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
        size_t scope_id = 0;

        size_t score = 0; // Lower is better
        size_t i     = 0;

        bool next(program_metadata const * meta, size_t param) {
          if (complete())
            return false;

          auto& arg = signature->arguments[i++];
          if (param != arg) {
            auto initializer = meta->find_unnamed_initializer(scope_id, arg, param);
            if (!initializer.has_value())
              return false; // No conversion available
            ++score;
          }

          return true;
        }

        bool complete() const {
          return i == signature->arguments.size();
        }
      };
    }

    std::optional<size_t> program_metadata::get_parameter_list_score(size_t scopeId, size_t funcType, ast const & ast, std::optional<size_t> const & paramList) const {
      if (!is_function(funcType)) {
        return std::nullopt;
      }
      auto decayed = decay_type(funcType);
      if (!decayed.has_value()) {
        return std::nullopt;
      }

      parameter_list_score_calculator scoreCalculator;
      scoreCalculator.signature = &std::get<type_function>(types[decayed.value()].desc);
      scoreCalculator.scope_id = scopeId;

      std::optional<size_t> current = paramList;
      while (current.has_value()) {
        auto & param = ast.get<expr::call_parameter>(current.value());
        if (!scoreCalculator.next(this, statement_info[param.expression].type_id.value())) {
          return std::nullopt; // Too many arguments
        }
        current = param.next;
      }

      if (!scoreCalculator.complete()) {
        return std::nullopt; // Not enough arguments
      }

      return scoreCalculator.score;
    }

    std::optional<size_t> program_metadata::get_parameter_list_score(size_t scopeId, size_t funcType, size_t const * paramList, size_t numParams) const {
      if (!is_function(funcType)) {
        return std::nullopt;
      }
      auto decayed = decay_type(funcType);
      if (!decayed.has_value()) {
        return std::nullopt;
      }

      parameter_list_score_calculator scoreCalculator;
      scoreCalculator.signature = &std::get<type_function>(types[decayed.value()].desc);
      scoreCalculator.scope_id = scopeId;

      for (size_t i = 0; i < numParams; ++i)
        if (!scoreCalculator.next(this, paramList[i]))
          return std::nullopt; // TOo many arguments

      if (!scoreCalculator.complete())
        return std::nullopt; // Not enough arguments

      return scoreCalculator.score;
    }

    std::optional<size_t> program_metadata::find_symbol(std::string_view const & fullName) const {
      auto found = std::find_if(symbols.begin(), symbols.end(), [&](symbol const& s) { return s.full_identifier == fullName; });
      if (found == symbols.end())
        return std::nullopt;
      return found - symbols.begin();
    }

    std::optional<size_t> program_metadata::find_unnamed_initializer(size_t scopeId, size_t receiverTypeIndex, size_t initializerTypeIndex) const {
      std::string_view fullName = adder::format(
        "init ([ref]%.*s,%.*s)=>void:",
        types[receiverTypeIndex].identifier.length(), types[receiverTypeIndex].identifier.data(),
        types[initializerTypeIndex].identifier.length(), types[initializerTypeIndex].identifier.data()
      );

      return search_for_symbol_index(scopeId, [fullName](symbol const & sym) {
        return sym.full_identifier == fullName;
      });
    }

    std::optional<size_t> program_metadata::get_parent_scope(size_t const & scopeId) const {
      if (scopeId < scopes.size())
        return scopes[scopeId].parent;
      else
        return std::nullopt;
    }

    vm::register_index program_builder::Registers::pin() {
      vm::register_index staleAvailble = (vm::register_index)vm::register_names::gp_end;
      size_t             staleLastUsed = useRound + 1;
      for (vm::register_index idx = 0; idx < (vm::register_index)vm::register_names::gp_end; ++idx) {
        if (states[idx].numPins != 0)
          continue;
        // if (!states[idx].value.has_value()) {
        //   return pin(idx);
        // }
        if (states[idx].lastUsed < staleLastUsed) {
          staleLastUsed = states[idx].lastUsed;
          staleAvailble = idx;
        }
      }
      assert(staleAvailble != (vm::register_index)vm::register_names::gp_end && "Failed to pin register");
      return pin(staleAvailble);
    }

    vm::register_index program_builder::Registers::pin(vm::register_index idx) {
      states[idx].lastUsed = ++useRound;
      ++states[idx].numPins;
      return idx;;
    }

    // std::optional<vm::register_index> program_builder::Registers::find_and_pin(const value & value) {
    //   for (vm::register_index idx = 0; idx < (vm::register_index)vm::register_names::count; ++idx) {
    //     auto & state = states[idx];
    //     if (!state.value.has_value())
    //       return std::nullopt;
    // 
    //     if (state.value->flags != value.flags) {
    //       return std::nullopt;
    //     }
    // 
    //     if (value.type_index.has_value() && state.value->type_index != value.type_index) {
    //       return std::nullopt;
    //     }
    // 
    //     if (value.constant.has_value() && state.value->constant == value.constant) {
    //       ++state.numPins;
    //       return idx;
    //     }
    // 
    //     if (value.indirect_register_index.has_value()) {
    //       if (state.value->indirect_register_index != value.indirect_register_index) {
    //         return std::nullopt;
    //       }
    //       if (state.value->address_offset != value.address_offset) {
    //         return std::nullopt;
    //       }
    //       return pin(idx);
    //     }
    // 
    //     if (value.symbol_index.has_value()) {
    //       if (state.value->symbol_index != value.symbol_index) {
    //         return std::nullopt;
    //       }
    //       if (state.value->address_offset != value.address_offset) {
    //         return std::nullopt;
    //       }
    //       return pin(idx);
    //     }
    //   }
    // 
    //   return std::nullopt;
    // }

    void program_builder::Registers::release(vm::register_index idx) {
      assert(states[idx].numPins > 0 && "register released too many times");
      --states[idx].numPins;
    }

    // void program_builder::Registers::evict()
    // {
    //   for (auto& state : states)
    //     state.value.reset();
    // }

    bool program_builder::begin_scope() {
      scopes.emplace_back();
      return true;
    }

    bool program_builder::end_scope() {
      assert(!scopes.empty());
      assert(scopes.back().temporaries.size() == 0);

      if (function_stack.size() > 0) {
        auto& func = current_function();
        auto& scope = scopes.back();
        for (auto it = scope.variables.rbegin(); it != scope.variables.rend(); ++it) {
          if ((it->flags & value_flags::alias) == value_flags::alias)
            continue;
          if ((it->flags & value_flags::stack_variable) == value_flags::stack_variable) {
            func.stack_storage_used -= meta.get_type_size(it->type_index.value());
          }
        }
      }

      scopes.pop_back();
      return true;
    }

    void program_builder::emit_scope_cleanup() {
      assert(!scopes.empty());

      emit_scope_cleanup(scopes.size() - 1);
    }

    void program_builder::emit_scope_cleanup(size_t upToScopeId) {
      assert(!scopes.empty());
      assert(scopes.back().temporaries.size() == 0);

      for (size_t scopeId = scopes.size() - 1; scopeId >= upToScopeId; --scopeId) {
        auto & scope = scopes[scopeId];
        for (auto it = scope.variables.rbegin(); it != scope.variables.rend(); ++it) {
          if ((it->flags & value_flags::alias) == value_flags::alias)
            continue;
          if ((it->flags & value_flags::stack_variable) == value_flags::stack_variable) {
            destroy_value(&(*it));
          }
        }
      }
    }

    void program_builder::push_return_handler(const std::function<void(program_builder*)> &handler) {
      return_handler_stack.push_back(handler);
    }
    
    void program_builder::pop_return_handler() {
      return_handler_stack.pop_back();
    }
    
    void program_builder::return_with_return_handler() {
      assert(!return_handler_stack.empty());
    
      return_handler_stack.back()(this);
    }

    program_builder::value program_builder::get_return_value() const {
      assert(!return_values.empty());
      return return_values.back();
    }

    void program_builder::push_return_value_receiver(value const& val) {
      return_values.push_back(val);
    }

    program_builder::value program_builder::pop_return_value_receiver() {
      assert(!return_values.empty());
      auto ret = return_values.back();
      return_values.pop_back();
      return ret;
    }
    
    size_t program_builder::get_value_type(value const & val) const {
      if (val.type_index.has_value()) {
        return val.type_index.value();
      }

      if (val.symbol_index.has_value()) {
        return meta.symbols[val.symbol_index.value()].type;
      }

      return 0;
    }

    void program_builder::add_variable(program_builder::value const & val) {
      assert(!scopes.empty());
      scopes.back().variables.push_back(val);
    }

    std::optional<program_builder::value> program_builder::find_unnamed_initializer(size_t receiver, size_t initializer) {
      std::string_view symbolName = adder::format(
        "init ([ref]%.*s,%.*s)=>void:",
        meta.types[receiver].identifier.length(), meta.types[receiver].identifier.data(),
        meta.types[initializer].identifier.length(), meta.types[initializer].identifier.data()
      );

      return find_value([=](program_builder::value const & candidate) {
        if (!candidate.symbol_index.has_value())
          return false;
        const auto & symbol = meta.symbols[candidate.symbol_index.value()];
        return symbol.full_identifier == symbolName;
      });
    }

    std::optional<program_builder::value> program_builder::find_operator(expr::operator_type op, size_t lhs, size_t rhs) {
      std::string_view opName = expr::get_operator_identifer(op);
      std::string_view symbolName = adder::format(
        "op (%.*s,%.*s)=>void:%.*s",
        meta.types[lhs].identifier.length(), meta.types[lhs].identifier.data(),
        meta.types[rhs].identifier.length(), meta.types[rhs].identifier.data(),
        opName.length(), opName.data()
      );

      return find_value([=](program_builder::value const & candidate) {
        if (!candidate.symbol_index.has_value())
          return false;
        const auto & symbol = meta.symbols[candidate.symbol_index.value()];
        return symbol.full_identifier == symbolName;
      });
    }

    std::optional<program_builder::value> program_builder::find_value_by_identifier(std::string_view const& name) const {
      return find_value_by_identifier(name, scopes.size() - 1);
    }

    std::optional<program_builder::value> program_builder::find_value_by_identifier(std::string_view const& name, size_t scopeIndex) const {
      return find_value([name](value const& val) { return val.identifier == name; }, scopeIndex);
    }

    std::optional<program_builder::value> program_builder::find_value(std::function<bool(value const&)> const & predicate) const {
      return find_value(predicate, scopes.size() - 1);
    }

    std::optional<program_builder::value> program_builder::find_value(std::function<bool(value const&)> const & predicate, size_t scopeIndex) const {
      assert(scopeIndex < scopes.size());
      const auto& scope     = scopes[scopeIndex];
      const auto& variables = scope.variables;
      auto found = std::find_if(variables.rbegin(), variables.rend(), predicate);
      if (found != variables.rend())
        return *found;
      if (scopeIndex == 0)
        return std::nullopt;
      return find_value(predicate, scopeIndex - 1);
    }

    program_builder::value program_builder::allocate_stack_variable(size_t typeIndex) {
      const size_t sz = meta.get_type_size(typeIndex);

      auto & func = current_function();

      value result;
      result.indirect_register_index = (vm::register_index)vm::register_names::fp;
      result.address_offset = func.stack_storage_used;
      result.type_index = typeIndex;
      result.flags |= value_flags::stack_variable;

      func.stack_storage_used += sz;
      func.max_stack_storage   = std::max(func.max_stack_storage, func.stack_storage_used);

      return result;
    }

    program_builder::value program_builder::allocate_temporary_value(size_t typeIndex) {
      const size_t sz = meta.get_type_size(typeIndex);

      auto & func = current_function();

      value result;
      result.indirect_register_index = (vm::register_index)vm::register_names::fp;
      result.address_offset = func.temp_storage_used;
      result.type_index = typeIndex;
      result.flags |= value_flags::temporary;

      func.temp_storage_used += sz;
      func.max_temp_storage   = std::max(func.max_temp_storage, func.temp_storage_used);

      scopes.back().temporaries.push_back(result);

      return result;
    }

    size_t program_builder::allocate_temporary_call_parameter(size_t typeIndex) {
      const size_t sz = meta.get_type_size(typeIndex);
      
      alloc_stack(sz);

      // Ammend other temporaries with addresses relative to the stack pointer
      for (auto& temporary : scopes.back().temporaries) {
        if (temporary.indirect_register_index == vm::register_names::sp) {
          temporary.address_offset -= sz;
        }
      }

      value result;
      result.type_index              = typeIndex;
      result.address_offset          = -(int64_t)sz;
      result.indirect_register_index = (vm::register_index)vm::register_names::sp;

      scopes.back().temporaries.push_back(result);

      return scopes.back().temporaries.size() - 1;
    }

    program_builder::value program_builder::get_temporary(size_t id) const {
      return scopes.back().temporaries[id];
    }

    void program_builder::free_temporary_value() {
      auto val = scopes.back().temporaries.back();
      destroy_value(&val);
      scopes.back().temporaries.pop_back();

      auto& func = current_function();
      const size_t sz = meta.get_type_size(val.type_index.value());
      if (val.indirect_register_index == vm::register_names::fp) {
        func.temp_storage_used -= sz;
      } 
      else if (val.indirect_register_index == vm::register_names::sp) {
        // func.call_params_used -= sz;

        // Ammend other temporaries with addresses relative to the stack pointer
        for (auto& temporary : scopes.back().temporaries) {
          if (temporary.indirect_register_index == vm::register_names::sp) {
            temporary.address_offset += sz;
          }
        }

        free_stack(sz);
      }
    }

    void program_builder::destroy_value(value * value) {
      unused(value);
    }

    size_t program_builder::current_scope_id() const {
      return functions[function_stack.back()].scope_id;
    }

    void program_builder::add_relocation(std::string_view const & symbol, uint64_t offset) {
      const size_t funcId = function_stack.back();
      const uint64_t base = sizeof(vm::instruction) * (functions[funcId].instructions.size() - 1);
      const uint64_t addr = base + offset;
      // TODO: Could be stored as a list of addresses per symbol.
      //       Might be more efficient when evaluating the relocations..
      relocations.push_back({ symbol, addr, function_stack.back() });
    }

    void program_builder::push_value(value r) {
      value_stack.push_back(std::move(r));
    }

    std::optional<program_builder::value> program_builder::pop_value() {
      if (value_stack.empty())
        return std::nullopt;

      auto ret = value_stack.back();
      value_stack.pop_back();
      return ret;
    }

    bool program_builder::begin_function(size_t symbolId) {
      program_metadata::symbol & symbol = meta.symbols[symbolId];

      if (!symbol.function_root_scope_id.has_value()) {
        // TODO: Push error. Cannot begin function body. No scope assocated with declaration. (possibly forward decl).
        return false;
      }

      function func;
      auto const & type = meta.types[symbol.type];
      auto const& funcDesc = std::get<type_function>(type.desc);
      for (size_t const & argTypes : funcDesc.arguments) {
        func.args_size += meta.get_type_size(argTypes);
      }
      func.return_type = funcDesc.return_type;
      func.symbol      = symbolId;
      func.scope_id    = symbol.function_root_scope_id.value();

      value rv;
      rv.indirect_register_index = (vm::register_index)vm::register_names::fp;
      rv.address_offset          = -(int64_t)meta.get_type_size(func.return_type) - func.args_size - function::CallLinkStorageSize;
      rv.type_index              = func.return_type;
      push_return_value_receiver(rv);

      function_stack.push_back(functions.size());
      functions.push_back(func);

      symbol.function_index = function_stack.back();

      return true;
    }

    void program_builder::end_function() {
      auto & func = current_function();

      // Process instruction tags
      for (size_t i = 0; i < func.instructions.size(); ++i) {
        auto& op = func.instructions[i];
        auto& tag = func.instruction_tags[i];
        switch (tag) {
        case instruction_tag::return_jmp: {
          // Jump to return statement
          assert(op.code == vm::op_code::jump_relative && "invalid op code tagged with instruction_tag::return_jmp");
          op.jump_relative.offset = (func.return_section_start - i) * sizeof(vm::instruction);
          break;
        }
        case instruction_tag::stack_frame: {
          uint32_t allocSize = (uint32_t)(func.max_stack_storage + func.max_temp_storage);
          switch (op.code) {
          case vm::op_code::alloc_stack:
            if (allocSize != 0)
              op.alloc_stack.bytes = allocSize;
            else
              op.code = vm::op_code::noop;
            break;
          case vm::op_code::free_stack:
            if (allocSize != 0)
              op.free_stack.bytes = allocSize;
            else
              op.code = vm::op_code::noop;
            break;
          default:
            assert(false && "invalid op code tagged with instruction_tag::stack_frame");
          }
          break;
        }
        case instruction_tag::add_temporary_storage_offset: {
          // Offset addresses by func.max_stack_storage.
          // Temporary storage is allocated after stack storage
          switch (op.code) {
          case vm::op_code::add_i64_constant:
            op.add_constant.rhs += func.max_stack_storage;
            break;
          case vm::op_code::set:
            op.set.val += func.max_stack_storage;
            break;
          case vm::op_code::load_offset:
            op.load_offset.offset += func.max_stack_storage;
            break;
          case vm::op_code::store_offset:
            op.store_offset.offset += func.max_stack_storage;
            break;
          case vm::op_code::store_value_offset:
            op.store_value_offset.offset += func.max_stack_storage;
            break;
          default:
            assert(false && "invalid op code tagged with instruction_tag::add_temporary_storage_offset");
          }
          break;
        }
        case instruction_tag::add_stack_storage_offset: {
          // no-op
          // stack storage is starts at frame-pointer + 0
          break;
        }
        default:
          break;
        }
      }

      function_stack.pop_back();
    }

    program_builder::function& program_builder::current_function() {
      assert(!function_stack.empty());
      return functions[function_stack.back()];
    }

    void program_builder::call(value const & func) {
      if (!meta.is_function(func.type_index))
        return;

      if (func.symbol_index.has_value()) {
        auto& symbol = meta.symbols[func.symbol_index.value()];

        if (symbol.function_index.has_value()) {
          call(0);
          add_relocation(symbol.full_identifier, AD_IOFFSET(call.addr));
        }
        else if (meta.is_reference(func.type_index)) {
          const auto addr = load_value_of(func);
          call_indirect(addr);
          registers.release(addr);
        }
      }
    }
    
    void program_builder::call(uint64_t address) {
      vm::instruction op;
      op.code = vm::op_code::call;
      op.call.addr = address;
      add_instruction(op);
    }

    void program_builder::ret() {
      vm::instruction op;
      op.code = vm::op_code::ret;
      add_instruction(op);
    }

    void program_builder::call_indirect(vm::register_index const & reg) {
      vm::instruction op;
      op.code = vm::op_code::call_indirect;
      op.call_indirect.addr = reg;
      add_instruction(op);
    }

    void program_builder::jump_to(value const & location) {
      if (!meta.is_function(location.type_index))
        return;

      if (meta.is_reference(location.type_index)) {
         vm::register_index addr = load_value_of(location);
         jump_indirect(addr);
         release_register(addr);
         return;
      }
      
      if (location.symbol_index.has_value()) {
        auto& symbol = meta.symbols[location.symbol_index.value()];

        // TODO: load address and jump indirect.

        jump_to(location.address_offset);
        add_relocation(symbol.full_identifier, AD_IOFFSET(jump.addr));
        return;
      }

      if (location.register_index.has_value()) {
        jump_indirect(location.register_index.value());
        return;
      }

      if (location.constant.has_value()) {
        jump_to(location.constant.value());
        return;
      }
    }

    void program_builder::jump_to(uint64_t address) {
      vm::instruction op;
      op.code = vm::op_code::jump;
      op.jump.addr = address;
      add_instruction(op);
    }

    void program_builder::jump_indirect(vm::register_index const & address) {
      vm::instruction op;
      op.code = vm::op_code::jump_indirect;
      op.jump_indirect.addr = address;
      add_instruction(op);
    }

    void program_builder::jump_relative(int64_t offset) {
      vm::instruction op;
      op.code = vm::op_code::jump_relative;
      op.jump_relative.offset = offset;
      add_instruction(op);
    }

    void program_builder::push_return_pointer() {
      push(vm::register_names::rp);
    }

    void program_builder::push_frame_pointer() {
      push(vm::register_names::fp);
    }

    void program_builder::pop_return_pointer() {
      pop(vm::register_names::rp);
    }

    void program_builder::pop_frame_pointer() {
      pop(vm::register_names::fp);
    }

    void program_builder::alloc_stack(size_t bytes) {
      if (bytes == 0)
        return;
      
      vm::instruction op;
      op.code = vm::op_code::alloc_stack;
      op.alloc_stack.bytes = (uint32_t)bytes;
      add_instruction(op);
    }

    void program_builder::free_stack(size_t bytes) {
      if (bytes == 0)
        return;

      vm::instruction op;
      op.code = vm::op_code::free_stack;
      op.free_stack.bytes = (uint32_t)bytes;
      add_instruction(op);
    }

    void program_builder::push(vm::register_index const & src) {
      vm::instruction op;
      op.code = vm::op_code::push;
      op.push.size = sizeof(vm::register_value);
      op.push.src  = src;
      add_instruction(op);
    }

    void program_builder::pop(vm::register_index const & dst) {
      vm::instruction op;
      op.code     = vm::op_code::pop;
      op.pop.size = sizeof(vm::register_value);
      op.pop.dst  = dst;
      add_instruction(op);
    }

    vm::register_index program_builder::pin_register() {
      return registers.pin();
    }

    vm::register_index program_builder::load_value_of(program_builder::value const & value) {
      if ((value.flags & program_builder::value_flags::eval_as_reference) != program_builder::value_flags::none) {
        return load_address_of(value);
      }

      if (value.register_index.has_value()) {
        // Might need some ref counting for "pin"
        return registers.pin(value.register_index.value());
      }

      if (value.constant.has_value()) {
        return load_constant(value.constant.value());
      }

      const size_t sz = meta.get_type_size(value.type_index.value());
      assert(sz <= sizeof(vm::register_value));

      if (value.indirect_register_index.has_value()) {
        vm::register_index ret = pin_register();
        load(ret, value.indirect_register_index.value(), sz, value.address_offset);

        if ((value.flags & value_flags::temporary) == value_flags::temporary)
          set_instruction_tag(instruction_tag::add_temporary_storage_offset);
        else if ((value.flags & value_flags::stack_variable) == value_flags::stack_variable)
          set_instruction_tag(instruction_tag::add_stack_storage_offset);

        return ret;
      }

      if (value.symbol_index.has_value()) {
        auto & symbol = meta.symbols[value.symbol_index.value()];
        assert(!symbol.has_local_storage() && "load_value_of is unable to locate local symbols");

        assert((symbol.flags & symbol_flags::extern_) == symbol_flags::none && "Extern not implemented (needs additional indirection)");

        vm::register_index ret = pin_register();
        load_from_constant_address(ret, value.address_offset, sz);
        add_relocation(symbol.full_identifier, AD_IOFFSET(load_addr.addr));
        return ret;
      }

      return 0;
    }

    vm::register_index program_builder::load_constant(vm::register_value value) {
      vm::register_index idx = registers.pin();
      set(idx, value);
      return idx;
    }

    vm::register_index program_builder::load_address_of(program_builder::value const & value) {
      if (value.constant.has_value()) {
        // TODO: Push error. Cannot get address of constant value
        assert(false);
        return 0;
      }

      if (value.indirect_register_index.has_value()) {
        vm::register_index ret = pin_register();
        addi_constant(ret, value.indirect_register_index.value(), value.address_offset);
        if ((value.flags & value_flags::temporary) == value_flags::temporary)
          set_instruction_tag(instruction_tag::add_temporary_storage_offset);
        else if ((value.flags & value_flags::stack_variable) == value_flags::stack_variable)
          set_instruction_tag(instruction_tag::add_stack_storage_offset);
        return ret;
      }

      if (value.symbol_index.has_value()) {
        auto & symbol = meta.symbols[value.symbol_index.value()];
        assert(!symbol.has_local_storage() && "load_address_of is unable to locate symbols with local storage");
        assert((symbol.flags & symbol_flags::extern_) == symbol_flags::none && "Extern not implemented (needs additional indirection)");

        vm::register_index ret = pin_register();
        set(ret, value.address_offset);
        add_relocation(symbol.full_identifier, AD_IOFFSET(set.val));
        return ret;
      }

      assert(false);
      return 0;
    }
    
    void program_builder::load(vm::register_index dst, vm::register_index address, size_t size, int64_t offset) {
      assert(dst < vm::register_count);
      assert(address < vm::register_count);
      assert(size <= sizeof(vm::register_value));

      vm::instruction op;
      op.code = vm::op_code::load_offset;
      op.load_offset.dst = dst;
      op.load_offset.src_addr = address;
      op.load_offset.size = (uint8_t)size;
      op.load_offset.offset = offset;
      add_instruction(op);
    }

    void program_builder::load(vm::register_index dst, vm::register_index address, size_t size) {
      assert(dst < vm::register_count);
      assert(address < vm::register_count);
      assert(size <= sizeof(vm::register_value));

      vm::instruction op;
      op.code = vm::op_code::load;
      op.load.dst = dst;
      op.load.src_addr = address;
      op.load.size = (uint8_t)size;
      add_instruction(op);
    }

    void program_builder::load_from_constant_address(vm::register_index dst, vm::register_value address, size_t size) {
      assert(dst < vm::register_count);
      assert(size <= sizeof(vm::register_value));

      vm::instruction op;
      op.code           = vm::op_code::load_addr;
      op.load_addr.dst  = dst;
      op.load_addr.addr = address;
      op.load_addr.size = (uint8_t)size;
      add_instruction(op);
    }

    void program_builder::release_register(vm::register_index reg) {
      return registers.release(reg);
    }

    void program_builder::move(vm::register_index dst, vm::register_index src) {
      vm::instruction i;
      i.code = vm::op_code::move;
      i.move.dst = dst;
      i.move.src = src;
      add_instruction(i);
    }

    void program_builder::set(vm::register_index dst, vm::register_value value) {
      vm::instruction i;
      i.code = vm::op_code::set;
      i.set.dst = dst;
      i.set.val = value;
      add_instruction(i);
    }

    bool program_builder::store(vm::register_index src, vm::register_index address, uint8_t sz) {
      if (sz > sizeof(vm::address_t))
        return false; // Too large.
      vm::instruction str;
      str.code       = vm::op_code::store;
      str.store.src  = src;
      str.store.addr = address;
      str.store.size = sz;
      add_instruction(str);
      return true;
    }

    bool program_builder::store(vm::register_index src, vm::register_index address, uint8_t sz, int64_t offset) {
      if (sz > sizeof(vm::address_t))
        return false; // Too large.
      vm::instruction str;
      str.code                = vm::op_code::store_offset;
      str.store_offset.src    = src;
      str.store_offset.addr   = address;
      str.store_offset.size   = sz;
      str.store_offset.offset = offset;
      add_instruction(str);
      return true;
    }

    bool program_builder::store_to_constant_address(vm::register_index src, vm::register_value dst, uint8_t sz) {
      if (sz > sizeof(vm::address_t))
        return false; // Too large.
      vm::instruction str;
      str.code             = vm::op_code::store_addr;
      str.store_addr.src   = src;
      str.store_addr.addr  = dst;
      str.store_addr.size  = sz;
      add_instruction(str);
      return true;
    }

    bool program_builder::store_constant_to_constant_address(vm::register_value src, vm::register_value dst, uint8_t sz) {
      vm::instruction str;
      str.code                  = vm::op_code::store_value_addr;
      str.store_value_addr.src  = src;
      str.store_value_addr.addr = dst;
      str.store_value_addr.size = sz;
      add_instruction(str);
      return true;
    }

    bool program_builder::store_constant(vm::register_value src, vm::register_index dst, uint8_t sz) {
      vm::instruction str;
      str.code             = vm::op_code::store_value;
      str.store_value.src  = src;
      str.store_value.addr = dst;
      str.store_value.size = sz;
      add_instruction(str);
      return true;
    }

    bool program_builder::store_constant(vm::register_value src, vm::register_index dst, uint8_t sz, int64_t offset) {
      vm::instruction str;
      str.code                      = vm::op_code::store_value_offset;
      str.store_value_offset.src    = src;
      str.store_value_offset.addr   = dst;
      str.store_value_offset.size   = sz;
      str.store_value_offset.offset = offset;
      add_instruction(str);
      return true;
    }

    bool program_builder::store_constant(vm::register_value src, program_builder::value const & dst) {
      if (dst.constant.has_value()) {
        assert(false && "Cannot store to constant value");
        return false;
      }

      if (!dst.type_index.has_value()) {
        return false;
      }

      const size_t sz = meta.get_type_size(dst.type_index.value());
      assert(sz <= sizeof(vm::register_value) && "value type does not fit in a register");
      if (dst.indirect_register_index.has_value()) {
        store_constant(src, dst.indirect_register_index.value(), (uint8_t)sz, dst.address_offset);
        if ((dst.flags & value_flags::temporary) == value_flags::temporary)
          set_instruction_tag(instruction_tag::add_temporary_storage_offset);
        else if ((dst.flags & value_flags::stack_variable) == value_flags::stack_variable)
          set_instruction_tag(instruction_tag::add_stack_storage_offset);
        return true;
      }

      if (dst.symbol_index.has_value()) {
        auto & symbol = meta.symbols[dst.symbol_index.value()];
        assert(!symbol.has_local_storage() && "load_address_of is unable to locate symbols with local storage");
        assert((symbol.flags & symbol_flags::extern_) == symbol_flags::none && "Extern not implemented (needs additional indirection)");
        store_constant_to_constant_address(src, dst.address_offset, (uint8_t)sz);
        add_relocation(symbol.full_identifier, AD_IOFFSET(store_value_addr.addr));
        return true;
      }

      assert(false);
      return false;
    }

    bool program_builder::store(vm::register_index src, program_builder::value const & dst) {
      if (dst.constant.has_value()) {
        assert(false && "Cannot store to constant value");
        return false;
      }

      if (!dst.type_index.has_value()) {
        return false;
      }

      const size_t sz = meta.get_type_size(dst.type_index.value());
      assert(sz <= sizeof(vm::register_value) && "value type does not fit in a register");
      if (dst.indirect_register_index.has_value()) {
        store(src, dst.indirect_register_index.value(), (uint8_t)sz, dst.address_offset);
        if ((dst.flags & value_flags::temporary) == value_flags::temporary)
          set_instruction_tag(instruction_tag::add_temporary_storage_offset);
        else if ((dst.flags & value_flags::stack_variable) == value_flags::stack_variable)
          set_instruction_tag(instruction_tag::add_stack_storage_offset);
        return true;
      }

      if (dst.symbol_index.has_value()) {
        auto & symbol = meta.symbols[dst.symbol_index.value()];
        assert(!symbol.has_local_storage() && "load_address_of is unable to locate symbols with local storage");
        assert((symbol.flags & symbol_flags::extern_) == symbol_flags::none && "Extern not implemented (needs additional indirection)");
        store_to_constant_address(src, dst.address_offset, (uint8_t)sz);
        add_relocation(symbol.full_identifier, AD_IOFFSET(store_addr.addr));
        return true;
      }

      assert(false);
      return 0;
    }

    // bool program_builder::store(program_builder::value const & src, program_builder::value const & dst) {
    //   if (src.constant.has_value()) {
    //     return store_constant(src.constant.value(), dst);
    //   }
    // 
    //   if (src.indirect_register_index.has_value()) {
    //     store(src, dst.indirect_register_index.value(), (uint8_t)sz, dst.address_offset);
    //     if ((dst.flags & value_flags::temporary) == value_flags::temporary)
    //       set_instruction_tag(instruction_tag::add_temporary_storage_offset);
    //     else if ((dst.flags & value_flags::stack_variable) == value_flags::stack_variable)
    //       set_instruction_tag(instruction_tag::add_stack_storage_offset);
    //     return true;
    //   }
    // 
    //   if (dst.symbol_index.has_value()) {
    //     auto & symbol = meta.symbols[dst.symbol_index.value()];
    //     assert(!symbol.has_local_storage() && "load_address_of is unable to locate symbols with local storage");
    //     assert((symbol.flags & symbol_flags::extern_) == symbol_flags::none && "Extern not implemented (needs additional indirection)");
    //     store_to_constant_address(src, dst.address_offset, (uint8_t)sz);
    //     add_relocation(symbol.full_identifier, AD_IOFFSET(store_addr.addr));
    //     return true;
    //   }
    // 
    // }

    void program_builder::addi(vm::register_index dst, vm::register_index a, vm::register_index b) {
      vm::instruction op;
      op.code = vm::op_code::add_i64;
      op.add.dst = dst;
      op.add.lhs = a;
      op.add.rhs = b;
      add_instruction(op);
    }

    void program_builder::addi_constant(vm::register_index dst, vm::register_index a, vm::register_value b) {
      vm::instruction op;
      op.code = vm::op_code::add_i64_constant;
      op.add_constant.dst = dst;
      op.add_constant.lhs = a;
      op.add_constant.rhs = b;
      add_instruction(op);
    }

    void program_builder::addf(vm::register_index dst, vm::register_index a, vm::register_index b) {
      vm::instruction op;
      op.code = vm::op_code::add_f64;
      op.add.dst = dst;
      op.add.lhs = a;
      op.add.rhs = b;
      add_instruction(op);
    }

    void program_builder::subi(vm::register_index dst, vm::register_index a, vm::register_index b) {
      vm::instruction op;
      op.code = vm::op_code::sub_i64;
      op.sub.dst = dst;
      op.sub.lhs = a;
      op.sub.rhs = b;
      add_instruction(op);
    }

    void program_builder::subf(vm::register_index dst, vm::register_index a, vm::register_index b) {
      vm::instruction op;
      op.code = vm::op_code::sub_f64;
      op.sub.dst = dst;
      op.sub.lhs = a;
      op.sub.rhs = b;
      add_instruction(op);
    }

    void program_builder::divi(vm::register_index dst, vm::register_index a, vm::register_index b) {
      vm::instruction op;
      op.code = vm::op_code::div_i64;
      op.div.dst = dst;
      op.div.lhs = a;
      op.div.rhs = b;
      add_instruction(op);
    }

    void program_builder::divf(vm::register_index dst, vm::register_index a, vm::register_index b) {
      vm::instruction op;
      op.code = vm::op_code::div_f64;
      op.div.dst = dst;
      op.div.lhs = a;
      op.div.rhs = b;
      add_instruction(op);
    }

    void program_builder::muli(vm::register_index dst, vm::register_index a, vm::register_index b) {
      vm::instruction op;
      op.code = vm::op_code::mul_i64;
      op.mul.dst = dst;
      op.mul.lhs = a;
      op.mul.rhs = b;
      add_instruction(op);
    }

    void program_builder::mulf(vm::register_index dst, vm::register_index a, vm::register_index b) {
      vm::instruction op;
      op.code = vm::op_code::mul_f64;
      op.mul.dst = dst;
      op.mul.lhs = a;
      op.mul.rhs = b;
      add_instruction(op);
    }

    void program_builder::add_instruction(vm::instruction inst) {
      assert(function_stack.size() > 0);

      auto& func = functions[function_stack.back()];
      func.instructions.push_back(inst);
      func.instruction_tags.push_back(instruction_tag::none);
    }

    void program_builder::set_instruction_tag(instruction_tag tag) {
      assert(function_stack.size() > 0);
      auto& func = functions[function_stack.back()];

      assert(func.instruction_tags.size() > 0);
      func.instruction_tags.back() = tag;
    }

    program program_builder::binary() const {
      // Compiled Program Layout
      // header
      //  * public_symbol_count: uint64_t
      //  * extern_symbol_count: uint64_t
      //  * symbol_data_size:    uint64_t
      //  * program_data_size:   uint64_t
      //  * code_size:           uint64_t
      // public_symbol_table[]
      // extern_symbol_table[]
      // symbol_data
      // program_data
      // code
      //
      // public_symbols is a sequence of symbol address/data address pairs.
      //   * symbol address is the location of the symbol name. Symbol name is a c-string
      //   * data address is the location of the data. Format of the data depends on the type of symbol.
      //     For a function, this is code.
      //     For a variable, this is the value and any initialization code.
      //   * public_symbols is terminated by a [ 0, 0 ] pair.
      //
      // extern_symbols is a sequence of address/symbol pairs.
      //   * Same as public_symbols except data address is 0 at program load time.
      //   * When the program is loaded, the vm should try resolve external symbols.
      //   * The VM will query the host for the symbol addresses and write the resolved address to "data address" in the table.

      program_header header;

      std::vector<program_symbol_table_entry> publicSymbols;
      std::vector<program_symbol_table_entry> externSymbols;
      std::vector<uint8_t> symbolData;
      std::vector<uint8_t> programData;
      std::map<std::string_view, uint64_t> symbolAddress;
      
      std::vector<vm::instruction> compiledCode;
      std::vector<uint64_t> functionOffset;
      functionOffset.resize(functions.size());

      for (auto& symbol : meta.symbols) {
        const bool isInline = (symbol.flags & symbol_flags::inline_) == symbol_flags::inline_;
        const bool isExtern = (symbol.flags & symbol_flags::extern_) == symbol_flags::extern_;
        if (symbol.has_local_storage() || symbol.is_parameter() || isInline) {
          continue;
        }

        program_symbol_table_entry item;
        item.name_address = symbolData.size();
        for (char c : symbol.full_identifier)
          symbolData.push_back(c);
        symbolData.push_back('\0');
        if (isExtern) {
          // TODO: For variable, lookup address via host.
          //       For function, generate code to call native method.
          item.data_address = 0;
          externSymbols.push_back(item);
        }
        else {
          if (meta.is_function(symbol.type) && symbol.function_index.has_value()) {
            auto& func = functions[symbol.function_index.value()];
            item.data_address = compiledCode.size() * sizeof(decltype(compiledCode)::value_type);
            functionOffset[symbol.function_index.value()] = compiledCode.size();
            compiledCode.insert(
              compiledCode.end(),
              func.instructions.begin(),
              func.instructions.end()
            );
          }
          else {
            size_t bytes = meta.get_type_size(symbol.type);
            // TODO: alignas(bytes)
            item.data_address = symbolData.size();
            symbolData.resize(symbolData.size() + bytes, 0);
          }
          publicSymbols.push_back(item);
        }
      }
      
      header.header_size = sizeof(program_header);
      
      header.public_symbol_count  = publicSymbols.size();
      header.public_symbol_offset = header.header_size;
      
      header.extern_symbol_count  = externSymbols.size();
      header.extern_symbol_offset = header.public_symbol_offset
        + header.public_symbol_count * sizeof(program_symbol_table_entry);
      
      header.symbol_data_size = symbolData.size();
      header.symbol_data_offset = header.extern_symbol_offset
        + header.extern_symbol_count * sizeof(program_symbol_table_entry);
      
      header.program_data_size   = 0;
      header.program_data_offset = header.symbol_data_offset + header.symbol_data_size;
      
      // TODO: alignas(vm::instruction)
      header.code_offset = header.program_data_offset + header.program_data_size;
      header.code_size   = compiledCode.size() * sizeof(vm::instruction);
      
      int64_t nextPublicEntry = 0;
      int64_t nextExternEntry = 0;
      for (auto & symbol : meta.symbols) {
        const bool isInline = (symbol.flags & symbol_flags::inline_) == symbol_flags::inline_;
        if (symbol.has_local_storage() || symbol.is_parameter() || isInline) {
          continue;
        }

        const bool isExtern = (symbol.flags & symbol_flags::extern_) == symbol_flags::extern_;
        if (isExtern) {
          externSymbols[nextExternEntry].name_address += header.symbol_data_offset;
      
          ++nextExternEntry;
        }
        else {
          publicSymbols[nextPublicEntry].name_address += header.symbol_data_offset;
      
          if (meta.is_function(symbol.type)) {
            publicSymbols[nextPublicEntry].data_address += header.code_offset;
          }
          else {
            publicSymbols[nextPublicEntry].data_address += header.symbol_data_offset;
          }
      
          symbolAddress[symbol.full_identifier] += publicSymbols[nextPublicEntry].data_address;
          ++nextPublicEntry;
        }
      }
      
      std::vector<uint8_t> executable;
      bytes::insert(executable, header);
      bytes::insert(executable, publicSymbols.begin(), publicSymbols.end());
      bytes::insert(executable, externSymbols.begin(), externSymbols.end());
      bytes::insert(executable, symbolData.begin(), symbolData.end());
      bytes::insert(executable, programData.begin(), programData.end());
      bytes::insert(executable, compiledCode.begin(), compiledCode.end());
      
      nextPublicEntry = 0;
      nextExternEntry = 0;
      
      for (auto& reloc : relocations) {
        const auto& func = meta.symbols[functions[reloc.function_id].symbol];
        memcpy(
          executable.data() + symbolAddress[func.full_identifier] + reloc.offset,
          &symbolAddress[reloc.symbol],
          sizeof(vm::address_t)
        );
      }

      // TODO: Below seems to be more efficient.
      //       Could be nice to get back to this impl by refactoring code and relocation storage a little.

      // for (auto & symbol : meta.symbols) {
      //   const bool isExtern = (symbol.flags & symbol_flags::extern_) == symbol_flags::extern_;
      //   if (isExtern)
      //     continue;
      // 
      //   if (meta.is_function(symbol.type)) {
      //     auto& func = functions[symbol.function_index.value()];
      //     
      //     for (size_t reloc = symbol.function->relocation_start; reloc < symbol.function->relocation_end; ++reloc) {
      //       auto& [name, offset] = relocations[reloc];
      // 
      //       uint64_t relativeOffset = offset - symbol.function->instruction_offset * sizeof(vm::instruction);
      //       memcpy(
      //         executable.data() + symbolAddress[symbol.name] + relativeOffset,
      //         &symbolAddress[name],
      //         sizeof(vm::address_t)
      //       );
      //     }
      //   }
      //   ++nextPublicEntry;
      // }
      
      return executable;
    }

    std::optional<std::string> get_type_name(ast const& ast, size_t statement) {
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
        auto& fn = ast.get<expr::type_fn>(statement);
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
        case functor_type::free:
          return ret + ")=>" + returnName.value();
        case functor_type::member:
          return "mem " + ret + ")=>" + returnName.value();
        case functor_type::initializer:
          return "init " + ret + ")=>" + returnName.value();
        case functor_type::operator_:
          return "op " + ret + ")=>" + returnName.value();
        }
      }

      return std::nullopt;
    }

    std::optional<std::string> get_symbol_name(ast const & ast, size_t statement, std::string_view const & identifier) {
      auto typeName = get_type_name(ast, statement);
      if (!typeName.has_value())
        return std::nullopt;

      return get_symbol_name(typeName.value(), identifier);
    }

    std::string get_symbol_name(std::string_view const & typeName, std::string_view const & identifier) {
      return (std::string)adder::format(
        "%.*s:%.*s",
        typeName.length(),
        typeName.data(),
        identifier.length(),
        identifier.data()
      );
    }
  }
}
