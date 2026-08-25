#include "compiler/program_builder.h"
#include "compiler.h"
#include "program.h"

namespace adder {
  namespace compiler {
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
            func.stack_storage_used -= it->type_info->get_size();
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
      for (size_t scopeId = scopes.size() - 1; scopeId >= upToScopeId; --scopeId) {
        auto & scope = scopes[scopeId];
        for (auto it = scope.variables.rbegin(); it != scope.variables.rend(); ++it) {
          if ((it->flags & value_flags::alias) == value_flags::alias)
            continue;
          if ((it->flags & value_flags::stack_variable) == value_flags::stack_variable) {
            destroy_value(&(*it));
          }
        }

        // Destroy any lingering temporaries
        for (auto it = scope.temporaries.rbegin(); it != scope.temporaries.rend(); ++it) {
          destroy_value(&(*it));
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
    
    type_reference program_builder::get_value_type(value const & val) const {
      if (val.type_info.has_value()) {
        return val.type_info.value();
      }

      if (val.symbol_index.has_value()) {
        return val.symbol_index->get_type();
      }

      return { meta.get(), type_id::undefined };
    }

    void program_builder::add_variable(program_builder::value const & val) {
      assert(!scopes.empty());
      scopes.back().variables.push_back(val);
    }

    std::optional<program_builder::value> program_builder::find_unnamed_initializer(type_reference receiver,
                                                                                    type_reference initializer) {
      std::string_view symbolName = adder::format("init ([ref]%.*s,%.*s)=>void:", receiver.get_identifier().length(),
                                                  receiver.get_identifier().data(), initializer.get_identifier().length(),
                                                  initializer.get_identifier().data());

      return find_value([=](program_builder::value const & candidate) {
        if (!candidate.symbol_index.has_value())
          return false;
        const auto & symbol = candidate.symbol_index->get();
        return symbol.full_identifier == symbolName;
      });
    }

    std::optional<program_builder::value> program_builder::find_operator(expr::operator_type op, type_reference lhs,
                                                                         type_reference rhs) {
      std::string_view opName = expr::get_operator_identifer(op);
      std::string_view symbolName = adder::format("op (%.*s,%.*s)=>void:%.*s", lhs.get_identifier().length(),
                                                  lhs.get_identifier().data(), rhs.get_identifier().length(),
                                                  rhs.get_identifier().data(), opName.length(), opName.data()
      );

      return find_value([=](program_builder::value const & candidate) {
        if (!candidate.symbol_index.has_value())
          return false;
        const auto & symbol = candidate.symbol_index->get();
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

    program_builder::value program_builder::allocate_stack_variable(type_reference typeInfo) {
      const size_t sz = typeInfo.get_size();

      auto & func = current_function();

      value result;
      result.indirect_register_index = (vm::register_index)vm::register_names::fp;
      result.address_offset          = func.stack_storage_used;
      result.type_info               = typeInfo;
      result.flags |= value_flags::stack_variable;

      func.stack_storage_used += sz;
      func.max_stack_storage = std::max(func.max_stack_storage, func.stack_storage_used);

      return result;
    }

    program_builder::value program_builder::allocate_temporary_value(type_reference typeInfo) {
      const size_t sz = typeInfo.get_size();

      auto & func = current_function();

      value result;
      result.indirect_register_index = (vm::register_index)vm::register_names::fp;
      result.address_offset          = func.temp_storage_used;
      result.type_info               = typeInfo;
      result.flags |= value_flags::temporary;

      func.temp_storage_used += sz;
      func.max_temp_storage = std::max(func.max_temp_storage, func.temp_storage_used);

      scopes.back().temporaries.push_back(result);

      return result;
    }

    size_t program_builder::allocate_temporary_call_parameter(type_reference typeInfo) {
      const size_t sz = typeInfo.get_size();
      
      alloc_stack(sz);

      // Ammend other temporaries with addresses relative to the stack pointer
      for (auto& temporary : scopes.back().temporaries) {
        if (temporary.indirect_register_index == vm::register_names::sp) {
          temporary.address_offset -= sz;
        }
      }

      value result;
      result.type_info               = typeInfo;
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

      auto &       func = current_function();
      const size_t sz   = val.type_info->get_size();
      if (val.indirect_register_index == vm::register_names::fp) {
        func.temp_storage_used -= sz;
      } else if (val.indirect_register_index == vm::register_names::sp) {
        // Ammend other temporaries with addresses relative to the stack pointer
        for (auto & temporary : scopes.back().temporaries) {
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
      return functions[function_stack.back()].scope_id.value();
    }

    void program_builder::add_relocation(program_metadata const * symbol_meta, std::string_view const & symbol, uint64_t offset) {
      relocation_linkage linkage = symbol_meta == meta.get() ? relocation_linkage::internal
                                                             : relocation_linkage::import_;

      return add_relocation(linkage, symbol_meta->module_name, symbol, offset);
    }

    void program_builder::add_relocation(relocation_linkage const & linkage, std::string_view const & module_name, std::string_view const & symbol, uint64_t offset) {
      const size_t funcId = function_stack.back();
      const uint64_t base = sizeof(vm::instruction) * (functions[funcId].instructions.size() - 1);
      const uint64_t addr = base + offset;
      // TODO: Could be stored as a list of addresses per symbol.
      //       Might be more efficient when evaluating the relocations.
      relocations.push_back({ linkage, module_name, symbol, addr, function_stack.back() });
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

    bool program_builder::begin_function(symbol_reference symbolId) {
      if (meta.get() != symbolId.meta)
        return false; // TODO: Error can't define method from another module

      symbol & symbol = meta->get(symbolId.index);

      function func;
      auto const & funcDesc = symbol.type.as<type_function>();
      for (type_reference const & argTypes : funcDesc.arguments) {
        func.args_size += argTypes.get_size();
      }
      func.return_type = funcDesc.return_type;
      func.symbol      = symbolId;
      func.scope_id    = symbol.function_root_scope_id;

      value rv;
      rv.indirect_register_index = (vm::register_index)vm::register_names::fp;
      rv.address_offset          = -(int64_t)func.return_type.get_size() - func.args_size - function::CallLinkStorageSize;
      rv.type_info               = func.return_type;

      push_return_value_receiver(rv);

      function_stack.push_back(functions.size());
      functions.push_back(func);

      symbol.function_index = function_stack.back();

      return true;
    }

    void program_builder::end_function() {
      auto & func = current_function();
      end_function(&func);
      function_stack.pop_back();
    }

    void program_builder::end_function(function* func) {
      // Process instruction tags
      for (size_t i = 0; i < func->instructions.size(); ++i) {
        auto& op = func->instructions[i];
        auto& tag = func->instruction_tags[i];
        switch (tag) {
        case instruction_tag::return_jmp: {
          // Jump to return statement
          assert(op.code == vm::op_code::jump_relative && "invalid op code tagged with instruction_tag::return_jmp");
          op.jump_relative.offset = (func->return_section_start - i) * sizeof(vm::instruction);
          break;
        }
        case instruction_tag::stack_frame: {
          uint32_t allocSize = (uint32_t)(func->max_stack_storage + func->max_temp_storage);
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
            op.add_constant.rhs += func->max_stack_storage;
            break;
          case vm::op_code::set:
            op.set.val += func->max_stack_storage;
            break;
          case vm::op_code::load_offset:
            op.load_offset.offset += func->max_stack_storage;
            break;
          case vm::op_code::store_offset:
            op.store_offset.offset += func->max_stack_storage;
            break;
          case vm::op_code::store_value_offset:
            op.store_value_offset.offset += func->max_stack_storage;
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
    }

    program_builder::function& program_builder::current_function() {
      assert(!function_stack.empty());
      return functions[function_stack.back()];
    }

    void program_builder::call(value const & func) {
      if (!is_function(func.type_info))
        return;

      if (func.symbol_index.has_value()) {
        auto & symbol = func.symbol_index->get();
        if (is_reference(func.type_info)) {
          const auto addr = load_value_of(func);
          call_indirect(addr);
          registers.release(addr);
        } else {
          call(0);
          add_relocation(func.symbol_index->meta, symbol.full_identifier, AD_IOFFSET(call.addr));
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

    void program_builder::call_native(symbol_reference const & symbol) {
      vm::instruction op;
      op.code = vm::op_code::call_native;
      op.call_native.native_method_index = 0;
      add_instruction(op);
      add_relocation(relocation_linkage::extern_, symbol.meta->module_name, symbol.get().full_identifier,
                     AD_IOFFSET(call_native.native_method_index));
    }

    void program_builder::jump_to(value const & location) {
      if (!is_function(location.type_info))
        return;

      if (is_reference(location.type_info)) {
         vm::register_index addr = load_value_of(location);
         jump_indirect(addr);
         release_register(addr);
         return;
      }
      
      if (location.symbol_index.has_value()) {
        auto& symbol = location.symbol_index->get();

        // TODO: load address and jump indirect.

        jump_to(location.address_offset);
        add_relocation(relocation_linkage::internal, location.symbol_index->meta->module_name, symbol.full_identifier,
                       AD_IOFFSET(jump.addr));
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

    void program_builder::jump_if_zero_rel(int64_t offset, vm::register_index dst) {
      vm::instruction op;
      op.code = vm::op_code::jump_if_zero_relative;
      op.jump_if_zero_relative.offset = offset;
      op.jump_if_zero_relative.cmp    = dst;
      add_instruction(op);
    }
    
    void program_builder::comparei(vm::register_index dst, vm::register_index a, vm::register_index b) {
      vm::instruction op;
      op.code = vm::op_code::compare_i64;
      op.compare.dst = dst;
      op.compare.lhs = a;
      op.compare.rhs = b;
      add_instruction(op);
    }

    void program_builder::comparef(vm::register_index dst, vm::register_index a, vm::register_index b) {
      vm::instruction op;
      op.code = vm::op_code::compare_f64;
      op.compare.dst = dst;
      op.compare.lhs = a;
      op.compare.rhs = b;
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

      // Just amend previous instruction if it was also an "alloc"
      // TODO: We need to know that "this" instruction won't be tagged either.
      //       Probably need to pass the tag into alloc_stack
      // auto & func = current_function();
      // if (!func.instructions.empty()
      //   && func.instructions.back().code == vm::op_code::alloc_stack
      //   && func.instruction_tags.back() == instruction_tag::none) {
      //   func.instructions.back().alloc_stack.bytes +=  (uint32_t)bytes;
      //   return;
      // }
      
      vm::instruction op;
      op.code = vm::op_code::alloc_stack;
      op.alloc_stack.bytes = (uint32_t)bytes;
      add_instruction(op);
    }

    void program_builder::free_stack(size_t bytes) {
      if (bytes == 0)
        return;

      // Just amend previous instruction if it was also a "free"
      // TODO: We need to know that "this" instruction won't be tagged either.
      //       Probably need to pass the tag into free_stack
      // auto & func = current_function();
      // if (!func.instructions.empty()
      //   && func.instructions.back().code == vm::op_code::free_stack
      //   && func.instruction_tags.back() == instruction_tag::none) {
      //   func.instructions.back().free_stack.bytes += (uint32_t)bytes;
      //   return;
      // }

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

      const size_t sz = get_size(value.type_info);
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
        auto & symbol = value.symbol_index->get();
        assert(!symbol.has_local_storage() && "load_value_of is unable to locate local symbols");

        assert((symbol.flags & symbol_flags::extern_) == symbol_flags::none && "Extern not implemented (needs additional indirection)");

        vm::register_index ret = pin_register();
        load_from_constant_address(ret, value.address_offset, sz);
        add_relocation(value.symbol_index->meta, symbol.full_identifier, AD_IOFFSET(load_addr.addr));
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
        auto & symbol = value.symbol_index->get();
        assert(!symbol.has_local_storage() && "load_address_of is unable to locate symbols with local storage");
        assert((symbol.flags & symbol_flags::extern_) == symbol_flags::none && "Extern not implemented (needs additional indirection)");

        vm::register_index ret = pin_register();
        set(ret, value.address_offset);
        add_relocation(value.symbol_index->meta, symbol.full_identifier, AD_IOFFSET(set.val));
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

    void program_builder::itof(vm::register_index dst, vm::register_index src, uint8_t fltSize) {
      vm::instruction i;
      switch (fltSize)
      {
      case 8:
        i.code = vm::op_code::itof64;
        break;
      case 4:
        i.code = vm::op_code::itof32;
        break;
      default:
        assert(false && "size not supported");
        break;
      }
      i.xtox.dst = dst;
      i.xtox.src = src;
      add_instruction(i);
    }

    void program_builder::ftoi(vm::register_index dst, vm::register_index src, uint8_t fltSize) {
      vm::instruction i;
      switch (fltSize)
      {
      case 8:
        i.code = vm::op_code::f64toi;
        break;
      case 4:
        i.code = vm::op_code::f32toi;
        break;
      default:
        assert(false && "size not supported");
        break;
      }
      i.xtox.dst = dst;
      i.xtox.src = src;
      add_instruction(i);
    }

    bool program_builder::store(vm::register_index src, vm::register_index address, uint8_t sz) {
      assert(sz <= sizeof(vm::register_value));
      vm::instruction str;
      str.code       = vm::op_code::store;
      str.store.src  = src;
      str.store.addr = address;
      str.store.size = sz;
      add_instruction(str);
      return true;
    }

    bool program_builder::store(vm::register_index src, vm::register_index address, uint8_t sz, int64_t offset) {
      assert(sz <= sizeof(vm::register_value));
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
      assert(sz <= sizeof(vm::register_value));
      vm::instruction str;
      str.code             = vm::op_code::store_addr;
      str.store_addr.src   = src;
      str.store_addr.addr  = dst;
      str.store_addr.size  = sz;
      add_instruction(str);
      return true;
    }

    bool program_builder::store_constant_to_constant_address(vm::register_value src, vm::register_value dst, uint8_t sz) {
      assert(sz <= sizeof(vm::register_value));
      vm::instruction str;
      str.code                  = vm::op_code::store_value_addr;
      str.store_value_addr.src  = src;
      str.store_value_addr.addr = dst;
      str.store_value_addr.size = sz;
      add_instruction(str);
      return true;
    }

    bool program_builder::store_constant(vm::register_value src, vm::register_index dst, uint8_t sz) {
      assert(sz <= sizeof(vm::register_value));
      vm::instruction str;
      str.code             = vm::op_code::store_value;
      str.store_value.src  = src;
      str.store_value.addr = dst;
      str.store_value.size = sz;
      add_instruction(str);
      return true;
    }

    bool program_builder::store_constant(vm::register_value src, vm::register_index dst, uint8_t sz, int64_t offset) {
      assert(sz <= sizeof(vm::register_value));
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

      if (is_undefined(dst.type_info)) {
        return false;
      }

      const size_t sz = get_size(dst.type_info);
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
        auto & symbol = dst.symbol_index->get();
        assert(!symbol.has_local_storage() && "load_address_of is unable to locate symbols with local storage");
        assert((symbol.flags & symbol_flags::extern_) == symbol_flags::none && "Extern not implemented (needs additional indirection)");
        store_constant_to_constant_address(src, dst.address_offset, (uint8_t)sz);
        add_relocation(dst.symbol_index->meta, symbol.full_identifier, AD_IOFFSET(store_value_addr.addr));
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

      if (is_undefined(dst.type_info)) {
        return false;
      }

      const size_t sz = get_size(dst.type_info);
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
        auto & symbol = dst.symbol_index->get();
        assert(!symbol.has_local_storage() && "load_address_of is unable to locate symbols with local storage");
        assert((symbol.flags & symbol_flags::extern_) == symbol_flags::none && "Extern not implemented (needs additional indirection)");
        store_to_constant_address(src, dst.address_offset, (uint8_t)sz);
        add_relocation(dst.symbol_index->meta, symbol.full_identifier, AD_IOFFSET(store_addr.addr));
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
    //     auto & symbol = meta->symbols[dst.symbol_index.value()];
    //     assert(!symbol.has_local_storage() && "load_address_of is unable to locate symbols with local storage");
    //     assert((symbol.flags & symbol_flags::extern_) == symbol_flags::none && "Extern not implemented (needs additional indirection)");
    //     store_to_constant_address(src, dst.address_offset, (uint8_t)sz);
    //     add_relocation(symbol.full_identifier, AD_IOFFSET(store_addr.addr));
    //     return true;
    //   }
    // }

    void program_builder::bitwise_and(vm::register_index dst, vm::register_index val) {
      vm::instruction op;
      op.code = vm::op_code::bitwise_and;
      op.bitwise_op.lhs = dst;
      op.bitwise_op.rhs = val;
      add_instruction(op);
    }

    void program_builder::bitwise_or(vm::register_index dst, vm::register_index val) {
      vm::instruction op;
      op.code = vm::op_code::bitwise_or;
      op.bitwise_op.lhs = dst;
      op.bitwise_op.rhs = val;
      add_instruction(op);
    }

    void program_builder::bitwise_xor(vm::register_index dst, vm::register_index val) {
      vm::instruction op;
      op.code = vm::op_code::bitwise_xor;
      op.bitwise_op.lhs = dst;
      op.bitwise_op.rhs = val;
      add_instruction(op);
    }
      
    void program_builder::bitwise_and_constant(vm::register_index dst, vm::register_value val) {
      vm::instruction op;
      op.code = vm::op_code::bitwise_and_value;
      op.bitwise_op_constant.reg = dst;
      op.bitwise_op_constant.val = val;
      add_instruction(op);
    }

    void program_builder::bitwise_or_constant(vm::register_index dst, vm::register_value val) {
      vm::instruction op;
      op.code = vm::op_code::bitwise_or_value;
      op.bitwise_op_constant.reg = dst;
      op.bitwise_op_constant.val = val;
      add_instruction(op);
    }

    void program_builder::bitwise_xor_constant(vm::register_index dst, vm::register_value val) {
      vm::instruction op;
      op.code = vm::op_code::bitwise_xor_value;
      op.bitwise_op_constant.reg = dst;
      op.bitwise_op_constant.val = val;
      add_instruction(op);
    }

    void program_builder::set_non_zero(vm::register_index reg, uint8_t ifNonZero, uint8_t ifZero) { {
      vm::instruction op;
      op.code = vm::op_code::set_non_zero;
      op.set_non_zero.dst = reg;
      op.set_non_zero.if_non_zero = ifNonZero;
      op.set_non_zero.if_zero = ifZero;
      add_instruction(op);
    }}

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
      auto& func = current_function();
      func.instructions.push_back(inst);
      func.instruction_tags.push_back(instruction_tag::none);
    }

    void program_builder::set_instruction_tag(instruction_tag tag) {
      auto& func = current_function();
      assert(func.instruction_tags.size() > 0);
      func.instruction_tags.back() = tag;
    }

    program program_builder::binary() const {
      // Compiled Program Layout
      // header: see program_header
      // public_symbol_table[]
      // relocation_table[]
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

      std::vector<program_symbol_table_entry> symbolTable;
      std::vector<uint8_t> relocationTable;
      std::vector<uint8_t> symbolData;
      std::vector<uint8_t> programData;
      std::map<std::string_view, uint64_t> symbolAddress;
      std::map<std::string_view, uint64_t> symbolIndices;
      
      std::vector<vm::instruction> compiledCode;
      std::vector<uint64_t> functionOffset;
      functionOffset.resize(functions.size());

      for (auto& ref : meta->symbol_references) {
        auto &     symbol   = ref.get();
        const bool isInline = (symbol.flags & symbol_flags::inline_) == symbol_flags::inline_;
        const bool isExtern = (symbol.flags & symbol_flags::extern_) == symbol_flags::extern_;
        const bool isImport = (symbol.flags & symbol_flags::import_) == symbol_flags::import_;
        if (symbol.has_local_storage() || symbol.is_parameter() || isInline || isImport) {
          continue;
        }

        program_symbol_table_entry item;
        item.name_address = symbolData.size();
        for (char c : symbol.full_identifier)
          symbolData.push_back(c);
        symbolData.push_back('\0');
        for (char c : ref.meta->module_name)
          symbolData.push_back(c);
        symbolData.push_back('\0');

        if (symbol.type.is_function() && symbol.function_index.has_value()) {
          // Stub is generated for functions with extern linkage.
          // Relocation linkage handles address resolution
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
          if (isExtern) {
            item.data_address = 0;
          }
          else {
            size_t bytes = symbol.type.get_size();
            // TODO: alignas(bytes)
            item.data_address = symbolData.size();
            symbolData.resize(symbolData.size() + bytes, 0);
          }
        }
        symbolIndices[symbol.full_identifier] = symbolTable.size();
        symbolTable.push_back(item);
      }

      const auto write_relocations = [&]() { // 
        std::map<relocation_linkage, std::map<std::string_view, std::vector<size_t>>> reloc_table;
        for (auto& reloc : relocations) {
          const auto& func = functions[reloc.function_id].symbol.get();
          reloc_table[reloc.linkage][reloc.symbol].push_back(symbolAddress[func.full_identifier] + reloc.offset);
        }

        relocationTable.clear();
        for (auto& [linkage, symbol_relocs] : reloc_table) {
          for (auto & [name, offsets] : symbol_relocs) {
            program_relocation_table_entry entry;
            entry.symbol  = symbolIndices[name];
            entry.count   = offsets.size();
            entry.linkage = linkage;
            bytes::insert(relocationTable, entry);
            for (auto& address : offsets)
              bytes::insert(relocationTable, address);
          }
        }
      };
      
      // Write relocation table so we know how big it is.
      // Need to know this so we can write the correct offsets in the header and calculate symbol addresses correctly.
      write_relocations();

      header.header_size = sizeof(program_header);
      
      header.symbol_count        = symbolTable.size();
      header.symbol_table_offset = header.header_size;
      header.symbol_table_offset += bytes::calc_align_padding(alignof(program_symbol_table_entry), header.symbol_table_offset);

      header.relocation_table_size   = relocationTable.size();
      header.relocation_table_offset = header.symbol_table_offset
        + header.symbol_count * sizeof(program_symbol_table_entry);

      header.symbol_data_size   = symbolData.size();
      header.symbol_data_offset = header.relocation_table_offset + header.relocation_table_size;

      header.program_data_size   = 0;
      header.program_data_offset = header.symbol_data_offset + header.symbol_data_size;
      
      header.code_offset  = header.program_data_offset + header.program_data_size;
      header.code_offset += bytes::calc_align_padding(alignof(vm::instruction), header.code_offset);
      header.code_size    = compiledCode.size() * sizeof(vm::instruction);

      int64_t nextEntry = 0;
      for (auto & ref : meta->symbol_references) {
        auto &     symbol   = ref.get();
        const bool isInline = (symbol.flags & symbol_flags::inline_) == symbol_flags::inline_;
        const bool isImport = (symbol.flags & symbol_flags::import_) == symbol_flags::import_;
        if (symbol.has_local_storage() || symbol.is_parameter() || isInline || isImport) {
          continue;
        }

        symbolTable[nextEntry].name_address += header.symbol_data_offset;
        if (symbol.type.is_function()) {
          symbolTable[nextEntry].data_address += header.code_offset;
        }
        else {
          const bool isExtern = (symbol.flags & symbol_flags::extern_) == symbol_flags::extern_;
          if (!isExtern) {
            symbolTable[nextEntry].data_address += header.symbol_data_offset;
          }
        }
        symbolAddress[symbol.full_identifier] += symbolTable[nextEntry].data_address;
        ++nextEntry;
      }
      
      // Now all the symbol addresses are final, write the final relocation table.
      write_relocations();

      // Assemble the program
      std::vector<uint8_t> executable;
      bytes::insert(executable, header);
      executable.resize(header.symbol_table_offset, 0);
      bytes::insert(executable, symbolTable.begin(), symbolTable.end());
      executable.resize(header.relocation_table_offset, 0);
      bytes::insert(executable, relocationTable.begin(), relocationTable.end());
      executable.resize(header.symbol_data_offset, 0);
      bytes::insert(executable, symbolData.begin(), symbolData.end());
      executable.resize(header.program_data_offset, 0);
      bytes::insert(executable, programData.begin(), programData.end());
      executable.resize(header.code_offset, 0);
      bytes::insert(executable, compiledCode.begin(), compiledCode.end());

      return executable;
    }
  }
}
