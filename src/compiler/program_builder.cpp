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

    std::optional<size_t> program_metadata::add_symbol(size_t scopeId, symbol const & s) {
      if (scopeId >= scopes.size())
        return std::nullopt;

      for (const size_t existingId : scopes[scopeId].symbols) {
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
      scopes[scopeId].symbols.push_back(symbolIndex);

      return symbolIndex;
    }

    std::optional<size_t> program_metadata::search_for_symbol_index(size_t scopeId, std::string_view const & identifier) const {
      return search_for_symbol_index(scopeId, [&identifier](symbol const& s) {
        return s.name == identifier;
      });
    }

    std::optional<size_t> program_metadata::search_for_symbol_index(size_t scopeId, std::function<bool(symbol const &)> const & pred) const {
      auto found = std::find_if(scopes[scopeId].symbols.rbegin(), scopes[scopeId].symbols.rend(), [&](int64_t idx) { return pred(symbols[idx]); });
      if (found != scopes[scopeId].symbols.rend()) {
        return *found;
      }

      if (!scopes[scopeId].parent.has_value())
        return std::nullopt;

      return search_for_symbol_index(scopes[scopeId].parent.value(), pred);
    }

    std::optional<size_t> program_metadata::find_symbol(std::string_view const & fullName) const {
      auto found = std::find_if(symbols.begin(), symbols.end(), [&](symbol const& s) { return s.full_identifier == fullName; });
      if (found == symbols.end())
        return std::nullopt;
      return found - symbols.begin();
    }

    std::optional<size_t> program_metadata::find_unnamed_initializer(size_t scopeId, size_t receiverTypeIndex, size_t initializerTypeIndex) const {
      std::string_view symbol = adder::format(
        "init ([ref]%.*s,%.*s)=>void:",
        types[receiverTypeIndex].identifier.length(), types[receiverTypeIndex].identifier.data(),
        types[initializerTypeIndex].identifier.length(), types[initializerTypeIndex].identifier.data()
      );

      return search_for_symbol_index(scopeId, symbol);
    }

    std::optional<size_t> program_metadata::get_parent_scope(size_t const & scopeId) const {
      if (scopeId < scopes.size())
        return scopes[scopeId].parent;
      else
        return std::nullopt;
    }

    vm::register_index program_builder::Registers::pin() {
      vm::register_index idx = 0;
      if (free.size() == 0) {
        idx = next++;
      }
      else {
        idx = free.front();
        free.erase(free.begin());
      }
      return idx;
    }

    void program_builder::Registers::release(vm::register_index idx) {
      free.push_back(idx);
    }

    void program_builder::pop_result() {
      auto r = results.back();
      results.pop_back();

      if (r.alloc_size.has_value()) {
        vm::instruction alloc;
        alloc.code = vm::op_code::free_stack;
        alloc.alloc_stack.bytes = (uint32_t)r.alloc_size.value();
        add_instruction(alloc);

        // TODO: Destroy value
      }
    }

    void program_builder::push_result(expression_result r) {
      results.push_back(r);
    }

    bool program_builder::begin_scope(size_t scopeId) {
      return true;
    }

    bool program_builder::end_scope() {
      return true;
    }

    size_t program_builder::current_scope_id() const {
      return functions[function_stack.back()].scope_id;
    }

    program_builder::expression_result program_builder::alloc_temporary_value(size_t typeIndex) {
      const size_t typeSize = meta.get_type_size(typeIndex);

      vm::instruction op;
      op.code = vm::op_code::alloc_stack;
      op.alloc_stack.bytes = (uint32_t)typeSize;

      if (op.alloc_stack.bytes != 0)
        add_instruction(op);

      scope &block = scopes.back();

      expression_result r;
      r.address = stack_frame_offset{ block.stackSize };
      r.type_index = typeIndex;
      r.alloc_size = typeSize;
      block.stackSize += typeSize;

      return r;
    }

    void program_builder::add_relocation(std::string_view const & symbol, uint64_t offset) {
      if (scratchRelocations.empty()) {
        relocations.push_back({ symbol, offset });
      }
      else {
        scratchRelocations.back().push_back({ symbol, offset });
      }
    }

    void program_builder::begin_relocation_group() {
      scratchRelocations.emplace_back();
    }

    void program_builder::begin_function(size_t symbol, size_t scope_id) {
      meta.symbols[symbol].function_index = function_stack.back();
      
      function_stack.push_back(functions.size());
      functions.emplace_back();
      functions.back().symbol   = symbol;
      functions.back().scope_id = scope_id;
    }

    void program_builder::finish_function() {
      function_stack.pop_back();
    }

    std::pair<size_t, size_t> program_builder::end_relocation_group() {
      std::pair<size_t, size_t> range = { relocations.size(), relocations.size() + scratchRelocations.back().size() };
      relocations.insert(
        relocations.end(),
        scratchRelocations.back().begin(),
        scratchRelocations.back().end());
      scratchRelocations.pop_back();
      return range;
    }

    void program_builder::call(program_metadata::symbol const & symbol) {
      if (!meta.is_function(symbol.type))
        return;

      constexpr size_t offset = (size_t)&((vm::instruction*)0)->jump.addr;

      if (symbol.function_index.has_value()) {
        call(program_address{ 0 });
        add_relocation(symbol.name, AD_IOFFSET(jump.addr));
        uint64_t offset = (uint8_t*)&code.back().jump.addr - (uint8_t*)code.data();
      }
      else {
        vm::register_index addr = pin_symbol(symbol);
        call_indirect(addr);
      }
    }

    void program_builder::call(program_address const & address) {
      vm::instruction op;
      op.code = vm::op_code::call;
      op.call.addr = address.addr;
      add_instruction(op);
    }

    void program_builder::call_indirect(address_desc const & addr) {
      vm::register_index idx = pin_address(addr, sizeof(vm::address_t));
      call_indirect(idx);
      release_register(idx);
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

    void program_builder::jump_to(program_metadata::symbol const& symbol) {
      if (!meta.is_function(symbol.type))
        return;

      if (meta.is_reference(symbol.type)) {
         vm::register_index addr = pin_symbol(symbol);
         jump_indirect(addr);
         release_register(addr);
         return;
      }

      if (symbol.address.has_value()) {
      }
      else {
        if (symbol.function.has_value()) {
          jump_to(program_address{ 0 });
          uint64_t offset = (uint8_t*)&code.back().jump.addr - (uint8_t*)code.data();
          add_relocation(symbol.name, offset);
        }
        else if (meta.is_reference(symbol.type_index)) {
        }
      }
    }

    void program_builder::jump_to(program_address const& address)
    {
      vm::instruction op;
      op.code = vm::op_code::jump;
      op.jump.addr = address.addr;
      add_instruction(op);
    }

    void program_builder::jump_indirect(address_desc const & addr)
    {
      vm::register_index idx = pin_address(addr, sizeof(vm::address_t));
      jump_indirect(idx);
      release_register(idx);
    }

    void program_builder::jump_indirect(vm::register_index const & address)
    {
      vm::instruction op;
      op.code = vm::op_code::jump_indirect;
      op.jump_indirect.addr = address;
      add_instruction(op);
    }

    void program_builder::push_return_pointer()
    {
      push(vm::register_names::rp);
    }

    void program_builder::push_frame_pointer()
    {
      push(vm::register_names::fp);
    }

    void program_builder::pop_return_pointer()
    {
      pop(vm::register_names::rp);
    }

    void program_builder::pop_frame_pointer()
    {
      pop(vm::register_names::fp);
    }

    // std::vector<Registers> registerStack;
    // /// Push current register state
    // bool push_registers() {
    //   registers.free;
    //   registerStack.push_back(registers);
    // }
    // 
    // /// Pop a previously pushed register state
    // bool pop_registers() {
    //   auto& state = registerStack.back();
    // 
    //   registers = state;
    //   registerStack.pop_back();
    // }

    bool program_builder::push_return_value_alias(std::string_view const & name, size_t typeIndex, symbol_flags const & flags) {
      symbol symbol;
      symbol.flags       = flags | symbol_flags::const_ | symbol_flags::fn_parameter;
      symbol.type_index  = typeIndex;
      symbol.name        = name;

      // Variable starts at the bottom of the stack (so frame pointer - frame size)
      // Variable ends at (frame pointer - frame size + variable size)
      // We offset from frame pointer as it is static during a scope/call. Stack pointer is always moving.
      symbol.address = stack_frame_offset{ -(int64_t)meta.get_type_size(typeIndex) };

      identifier id;
      id.symbol_index = push_symbol(symbol);
      id.name = name;
      if (!id.symbol_index.has_value())
        return false;

      scope &block = scopes.back();
      block.identifiers.push_back(id);
      return true;
    }

    bool program_builder::push_fn_parameter(std::string_view const & name, size_t typeIndex, symbol_flags const & flags) {
      symbol symbol;
      symbol.flags       = flags | symbol_flags::const_ | symbol_flags::fn_parameter;
      symbol.type_index  = typeIndex;
      symbol.name        = name;

      scope &block = scopes.back();
      block.stackSize += meta.get_type_size(typeIndex);

      // Variable starts at the bottom of the stack (so frame pointer - frame size)
      // Variable ends at (frame pointer - frame size + variable size)
      // We offset from frame pointer as it is static during a scope/call. Stack pointer is always moving.
      symbol.address = stack_frame_offset{ block.stackSize };

      identifier id;
      id.symbol_index = push_symbol(symbol);
      id.name = name;
      if (!id.symbol_index.has_value())
        return false;

      block.identifiers.push_back(id);
      return true;
    }

    bool program_builder::push_variable(std::string_view const& name, size_t typeIndex, symbol_flags const & flags) {
      auto const& type = meta.types[typeIndex];

      vm::instruction alloc;
      alloc.code = vm::op_code::alloc_stack;
      alloc.alloc_stack.bytes = (uint32_t)meta.get_type_size(type);
      add_instruction(alloc);

      symbol symbol;
      symbol.flags       = flags;
      symbol.type_index  = typeIndex;
      symbol.name        = name; // TODO: Generate more unique symbol names

      scope &block = scopes.back();
      symbol.address = stack_frame_offset{ block.stackSize };
      block.stackSize += meta.get_type_size(type);

      identifier id;
      id.symbol_index = push_symbol(symbol);
      id.name = name;
      if (!id.symbol_index.has_value())
        return false;

      block.identifiers.push_back(id);
      return true;
    }

    bool program_builder::push_variable(std::string_view const& identifier, std::string_view const & typeName, symbol_flags const & flags) {
      auto typeIndex = meta.get_type_index(typeName);
      return typeIndex.has_value() && push_variable(identifier, typeIndex.value(), flags);
    }

    bool program_builder::push_identifier(std::string_view const & name, symbol const & symbol) {
      scope &block = scopes.back();

      identifier id;
      id.symbol_index = push_symbol(symbol);
      id.name = name;
      if (!id.symbol_index.has_value())
        return false;

      if (!name.empty())
        block.identifiers.push_back(id);
      return true;
    }

    void program_builder::push_expression_result(expression_result result) {
      results.push_back(result);
    }

    void program_builder::push(vm::register_index const& src) {
      vm::instruction op;
      op.code = vm::op_code::push;
      op.push.size = sizeof(vm::register_value);
      op.push.src  = src;
      add_instruction(op);

      scope& block = scopes.back();
      block.stackSize += op.push.size;
    }

    void program_builder::pop(vm::register_index const& dst) {
      vm::instruction op;
      op.code     = vm::op_code::pop;
      op.pop.size = sizeof(vm::register_value);
      op.pop.dst  = dst;
      add_instruction(op);

      scope& block = scopes.back();
      block.stackSize += op.push.size;
    }

    vm::register_index program_builder::pin_register() {
      return registers.pin();
    }

    vm::register_index program_builder::pin_symbol(program_metadata::symbol const& symbol) {
      const size_t sz = meta.get_type_size(meta.types[symbol.type_index]);
      if (sz > sizeof(vm::register_value)) {
        // Error: Cannot pin `symbol` to register. Too large.
      }

      if (symbol.address.has_value()) {
        return pin_address(symbol.address.value(), sz);
      }
      else {
        return pin_relocation(symbol.name, sz);
      }
    }

    vm::register_index program_builder::pin_constant(vm::register_value value) {
      vm::register_index idx = registers.pin();
      vm::instruction op;
      op.code = vm::op_code::set;
      op.set.val = value;
      op.set.dst = idx;
      add_instruction(op);
      return idx;
    }

    vm::register_index program_builder::pin_address(program_address address, size_t size) {
      return pin_address(address.addr, size);
    }

    vm::register_index program_builder::pin_address(stack_frame_offset stack, size_t size) {
      return pin_stack_frame_offset(stack.offset, size);
    }

    vm::register_index program_builder::pin_address(address_desc address, size_t size) {
      return std::visit([=](auto&& o) { return pin_address(o, size); }, address);
    }

    vm::register_index program_builder::pin_stack_frame_offset(int64_t offset, size_t size) {
      vm::register_index idx = registers.pin();
      vm::instruction op;
      op.code = vm::op_code::load_offset;
      op.load_offset.addr   = (uint8_t)vm::register_names::fp;
      op.load_offset.offset = offset;
      op.load_offset.size   = (uint8_t)size;
      op.load_offset.dst    = idx;
      add_instruction(op);
      return idx;
    }

    vm::register_index program_builder::pin_address(uint64_t address, size_t size) {
      vm::register_index idx = registers.pin();
      vm::instruction op;
      op.code = vm::op_code::load_addr;
      op.load_addr.addr = address;
      op.load_addr.size = (uint8_t)size;
      op.load_addr.dst = idx;
      add_instruction(op);
      return idx;
    }

    vm::register_index program_builder::pin_result(expression_result const & value) {
      if (value.constant.has_value()) {
        return pin_constant(value.constant.value());
      }
      else if (value.address.has_value()) {
        return pin_address(value.address.value(), meta.get_type_size(value.type_index.value()));
      }
      else if (value.symbol_index.has_value()) {
        return pin_symbol(symbols[value.symbol_index.value()]);
      }
      return registers.pin();
    }

    vm::register_index program_builder::pin_address_of(program_metadata::symbol const & symbol) {
      if (symbol.address.has_value()) {
        vm::register_index dst = pin_register();
        set(dst, symbol.address.value());
        return dst;
      }
      else {
        vm::register_index dst = pin_register();
        set(dst, program_address{ 0 });
        uint64_t offset = (uint8_t*)&code.back().set.val - (uint8_t*)code.data();
        add_relocation(symbol.name, offset);
        return dst;
      }
    }

    vm::register_index program_builder::pin_address_of(expression_result const & result) {
      if (result.address.has_value()) {
        vm::register_index dst = pin_register();
        set(dst, result.address.value());
        return dst;
      }
      else if (result.symbol_index.has_value()) {
        return pin_address_of(symbols[result.symbol_index.value()]);
      }

      return false;
    }

    vm::register_index program_builder::pin_relocation(std::string_view identifier, size_t size) {
      vm::register_index idx = registers.pin();

      vm::instruction op;
      op.code = vm::op_code::load_addr;
      op.load_addr.addr = 0;
      op.load_addr.size = (uint8_t)size;
      op.load_addr.dst  = idx;
      add_instruction(op);

      // Record that this instruction will require `load_addr.addr` to be relocated.
      uint64_t offset = (uint8_t*)&code.back().load_addr.addr - (uint8_t*)code.data();
      add_relocation(identifier, offset);
      return idx;
    }
    
    void program_builder::load(vm::register_index dst, vm::register_index address, size_t size, int64_t offset)
    {
      vm::instruction op;
      op.code = vm::op_code::load_offset;
      op.load_offset.dst = dst;
      op.load_offset.addr = address;
      op.load_offset.size = (uint8_t)size;
      op.load_offset.offset = offset;
      add_instruction(op);
    }

    void program_builder::load(vm::register_index dst, vm::register_index address, size_t size)
    {
      vm::instruction op;
      op.code = vm::op_code::load;
      op.load.dst = dst;
      op.load.src_addr = address;
      op.load.size = (uint8_t)size;
      add_instruction(op);
    }

    void program_builder::release_register(vm::register_index reg) {
      return registers.release(reg);
    }

    program_builder::expression_result program_builder::pop_expression_result() {
      expression_result ret = results.back();
      results.pop_back();
      return ret;
    }

    void program_builder::move(vm::register_index dst, vm::register_index src)
    {
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

    void program_builder::set(vm::register_index dst, program_address const& addr) {
      set(dst, addr.addr);
    }

    void program_builder::set(vm::register_index dst, stack_frame_offset const& addr) {
      set(dst, addr.offset);

      vm::instruction add;
      add.code = vm::op_code::add_i64;
      add.add.dst = dst;
      add.add.lhs = dst;
      add.add.rhs = (uint8_t)vm::register_names::fp;
      add_instruction(add);
    }

    void program_builder::set(vm::register_index dst, address_desc const& addr) {
      std::visit([=](auto&& o) { return set(dst, o); }, addr);
    }

    bool program_builder::store(vm::register_index src, program_address const & addr, uint8_t sz) {
      vm::instruction instr;
      instr.code = vm::op_code::store_addr;
      instr.store_addr.addr = addr.addr;
      instr.store_addr.src      = src;
      instr.store_addr.size     = sz;
      add_instruction(instr);
      return true;
    }

    bool program_builder::store(vm::register_index src, stack_frame_offset const & addr, uint8_t sz) {
      vm::instruction instr;
      instr.code = vm::op_code::store_offset;
      instr.store_offset.addr   = (uint8_t)vm::register_names::fp;
      instr.store_offset.offset = addr.offset;
      instr.store_offset.src    = src;
      instr.store_offset.size   = sz;
      add_instruction(instr);
      return true;
    }

    bool program_builder::store(vm::register_index src, address_desc const & addr, uint8_t sz) {
      return std::visit([=](auto&& o) { return store(src, o, sz); }, addr);
    }

    bool program_builder::store(vm::register_index src, vm::register_index dst, uint8_t sz) {
      if (sz > sizeof(vm::address_t))
        return false; // Too large.
      vm::instruction str;
      str.code       = vm::op_code::store;
      str.store.src  = src;
      str.store.addr = dst;
      str.store.size = sz;
      add_instruction(str);
      return true;
    }

    bool program_builder::store(vm::register_index src, program_metadata::symbol const & symbol) {
      if (!store(src, symbol.address.value_or(program_address{ 0 }), (uint8_t)meta.get_type_size(meta.types[symbol.type_index])))
        return false;
      if (symbol.address.has_value())
        return true;

      // Add relocation
      vm::instruction &op = code.back();
      vm::register_value *pAddr = 0;
      switch (op.code)
      {
      case vm::op_code::store_addr:
        pAddr = &op.store_addr.addr;
        break;
      }

      add_relocation(
        symbol.name,
        (uint8_t*)pAddr - (uint8_t*)code.data()
      );
      return false;
    }

    bool program_builder::store(vm::register_index src, expression_result const& result) {
      if (result.symbol_index.has_value()) {
        return store(src, symbols[result.symbol_index.value()]);
      }
      else if (result.type_index.has_value()) {
        if (result.address.has_value()) {
          return store(src, result.address.value(), (uint8_t)meta.get_type_size(result.type_index.value()));
        }
        else if (result.constant.has_value()) {
          return false;
        }
      }

      return false;
    }

    void program_builder::addi(vm::register_index dst, vm::register_index a, vm::register_index b)
    {
      vm::instruction op;
      op.code = vm::op_code::add_i64;
      op.add.dst = dst;
      op.add.lhs = a;
      op.add.rhs = b;
      add_instruction(op);
    }
    
    void program_builder::addf(vm::register_index dst, vm::register_index a, vm::register_index b)
    {
      vm::instruction op;
      op.code = vm::op_code::add_f64;
      op.add.dst = dst;
      op.add.lhs = a;
      op.add.rhs = b;
      add_instruction(op);
    }

    void program_builder::add_instruction(vm::instruction inst) {
      current_function.instructions.push_back(inst);
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

      // program_header header;
      // 
      // std::vector<program_symbol_table_entry> publicSymbols;
      // std::vector<program_symbol_table_entry> externSymbols;
      // std::vector<uint8_t> symbolData;
      // std::vector<uint8_t> programData;
      // std::map<std::string_view, uint64_t> symbolAddress;
      // 
      // std::vector<vm::instruction> compiledCode;
      // 
      // for (auto& symbol : meta.symbols) {
      //   program_symbol_table_entry item;
      //   item.name_address = symbolData.size();
      //   for (char c : symbol.name)
      //     symbolData.push_back(c);
      //   symbolData.push_back('\0');
      //   const bool isExtern = (symbol.flags & symbol_flags::extern_) == symbol_flags::extern_;
      //   if (isExtern) {
      //     // TODO: For variable, lookup address via host.
      //     //       For function, generate code to call native method.
      //     item.data_address = 0;
      //     externSymbols.push_back(item);
      //   }
      //   else {
      //     if (meta.is_function(symbol.type_index)) {
      //       item.data_address = compiledCode.size() * sizeof(decltype(compiledCode)::value_type);
      //       compiledCode.insert(
      //         compiledCode.end(),
      //         code.begin() + symbol.function->instruction_offset,
      //         code.begin() + symbol.function->instruction_offset + symbol.function->instruction_count
      //       );
      //     }
      //     else {
      // 
      //       size_t bytes = meta.get_type_size(symbol.type_index);
      //       // TODO: alignas(bytes)
      //       item.data_address = symbolData.size();
      //       symbolData.resize(symbolData.size() + bytes, 0);
      //     }
      //     publicSymbols.push_back(item);
      //   }
      // }
      // 
      // header.header_size = sizeof(program_header);
      // 
      // header.public_symbol_count  = publicSymbols.size();
      // header.public_symbol_offset = header.header_size;
      // 
      // header.extern_symbol_count  = externSymbols.size();
      // header.extern_symbol_offset = header.public_symbol_offset
      //   + header.public_symbol_count * sizeof(program_symbol_table_entry);
      // 
      // header.symbol_data_size = symbolData.size();
      // header.symbol_data_offset = header.extern_symbol_offset
      //   + header.extern_symbol_count * sizeof(program_symbol_table_entry);
      // 
      // header.program_data_size   = 0;
      // header.program_data_offset = header.symbol_data_offset + header.symbol_data_size;
      // 
      // // TODO: alignas(vm::instruction)
      // header.code_offset = header.program_data_offset + header.program_data_size;
      // header.code_size   = compiledCode.size() * sizeof(vm::instruction);
      // 
      // int64_t nextPublicEntry = 0;
      // int64_t nextExternEntry = 0;
      // for (auto & symbol : meta.symbols) {
      //   const bool isExtern = (symbol.flags & symbol_flags::extern_) == symbol_flags::extern_;
      //   if (isExtern) {
      //     externSymbols[nextExternEntry].name_address += header.symbol_data_offset;
      // 
      //     ++nextExternEntry;
      //   }
      //   else {
      //     publicSymbols[nextPublicEntry].name_address += header.symbol_data_offset;
      // 
      //     if (meta.is_function(symbol.type_index)) {
      //       publicSymbols[nextPublicEntry].data_address += header.code_offset;
      //     }
      //     else {
      //       publicSymbols[nextPublicEntry].data_address += header.symbol_data_offset;
      //     }
      // 
      //     symbolAddress[symbol.name] += publicSymbols[nextPublicEntry].data_address;
      //     ++nextPublicEntry;
      //   }
      // }
      // 
      // std::vector<uint8_t> executable;
      // bytes::insert(executable, header);
      // bytes::insert(executable, publicSymbols.begin(), publicSymbols.end());
      // bytes::insert(executable, externSymbols.begin(), externSymbols.end());
      // bytes::insert(executable, symbolData.begin(), symbolData.end());
      // bytes::insert(executable, programData.begin(), programData.end());
      // bytes::insert(executable, compiledCode.begin(), compiledCode.end());
      // 
      // nextPublicEntry = 0;
      // nextExternEntry = 0;
      // 
      // for (auto & symbol : symbols) {
      //   const bool isExtern = (symbol.flags & symbol_flags::extern_) == symbol_flags::extern_;
      //   if (isExtern)
      //     continue;
      // 
      //   if (meta.is_function(symbol.type_index)) {
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
      // 
      // return executable;

      return {};
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
