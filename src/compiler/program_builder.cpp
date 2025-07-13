#include "program_builder.h"
#include "../compiler.h"

namespace adder {
  namespace compiler {
    program_builder::program_builder() {
      scopes.push_back({});
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

    size_t program_builder::get_type_index(std::string_view const& name) const {
      auto it = std::find_if(types.begin(), types.end(), [&](type const& t) { return t.identifier == name; });
      return it - types.begin();
    }

    type const * program_builder::get_type(std::string_view const& name) const {
      auto it = std::find_if(types.begin(), types.end(), [&](type const& t) { return t.identifier == name; });
      if (it == types.end())
        return nullptr;
      return &(*it);
    }

    size_t program_builder::get_type_index(ast const & tree, size_t type) const {
      auto name = get_type_name(tree, type);
      return name.has_value() ? get_type_index(name.value()) : 0;
    }

    type const * program_builder::get_type(ast const & tree, size_t type) const {
      auto name = get_type_name(tree, type);
      return name.has_value() ? get_type(name.value()) : nullptr;
    }

    bool program_builder::is_reference_of(size_t const & reference, size_t const & baseType) const {
      if (!std::holds_alternative<type_modifier>(types[reference].desc)) {
        return false;
      }

      auto& modifiers = std::get<type_modifier>(types[reference].desc);
      return modifiers.reference && modifiers.base == baseType;
    }

    bool program_builder::is_function(size_t const& type) const {
      return std::holds_alternative<type_function_decl>(types[type].desc);
    }

    bool program_builder::is_const(size_t const & type) const {
      if (!std::holds_alternative<type_modifier>(types[type].desc)) {
        return false;
      }
      return std::holds_alternative<type_modifier>(types[type].desc)
        && std::get<type_modifier>(types[type].desc).const_;
    }

    size_t program_builder::get_type_size(type_modifier const & desc) const {
      return desc.reference ? sizeof(vm::address_t) : get_type_size(desc.base);
    }

    size_t program_builder::get_type_size(type_primitive const& desc) const {
      switch (desc) {
      case type_primitive::_void: return 0;
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

    size_t program_builder::get_type_size(type_class const& desc) const {
      return desc.size;
    }

    size_t program_builder::get_type_size(type_function const& desc) const {
      return desc.size;
    }

    size_t program_builder::get_type_size(type_function_decl const& desc) const {
      return desc.size;
    }

    size_t program_builder::get_type_size(size_t const & typeIndex) const {
      return get_type_size(types[typeIndex]);
    }

    size_t program_builder::get_type_size(type const& type) const {
      return std::visit([this](auto const& o) {
        return get_type_size(o);
        }, type.desc);
    }

    std::optional<size_t> program_builder::find_symbol_index(std::string_view const& name) const {
      auto it = std::find_if(
        symbols.rbegin(),
        symbols.rend(),
        [&](const symbol &desc) { return desc.name == name; });
      return it == symbols.rend() ? -1 : &(*it) - symbols.data();
    }

    program_builder::symbol const * program_builder::find_symbol(std::string_view const& name) const {
      std::optional<size_t> idx = find_symbol_index(name);

      return idx.has_value() ? &symbols[idx.value()] : nullptr;
    }

    std::optional<size_t> program_builder::lookup_identifier_symbol_index(std::string_view const & identifier) const {
      auto it = std::find_if(
        identifiers.rbegin(),
        identifiers.rend(),
        [&](const symbol& desc) { return desc.name == identifier; });
      if (it == symbols.rend())
        return std::nullopt;

      return it->symbol_index;
    }

    program_builder::symbol const * program_builder::lookup_identifier_symbol(std::string_view const & identifier) const {
      std::optional<size_t> idx = lookup_identifier_symbol_index(identifier);

      return idx.has_value() ? &symbols[idx.value()] : nullptr;
    }

    std::optional<size_t> program_builder::find_unnamed_initializer(size_t receiverTypeIndex, size_t initializerTypeIndex) const {
      std::string_view symbol = adder::format(
        "init:([ref]%.*s,%.*s)=>void:",
        types[receiverTypeIndex].identifier.length(), types[receiverTypeIndex].identifier.data(),
        types[initializerTypeIndex].identifier.length(), types[initializerTypeIndex].identifier.data()
      );

      return find_symbol_index(symbol);
    }

    size_t program_builder::add_type(type const & desc) {
      if (get_type(desc.identifier) != nullptr)
        return false;
      types.push_back(desc);
      return types.size() - 1;
    }

    size_t program_builder::add_function_type(ast const& tree, expr::function_declaration const& decl, std::optional<size_t> id) {
      type_function_decl fn;
      fn.allowInline;
      fn.function_id = id.value();
      fn.type = get_type_index(tree, decl.type.value());

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

    bool program_builder::push_scope() {
      scopes.emplace_back();
      return true;
    }

    bool program_builder::pop_scope() {
      scopes.pop_back();
      return true;
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

    bool program_builder::push_symbol(symbol const & desc) {
      scope &block = scopes.back();
      bool isGlobal = (desc.flags & symbol_flags::extern_) == symbol_flags::extern_
        || (desc.flags & symbol_flags::extern_) == symbol_flags::static_
        || scopes.size() == 1;

      symbol newSymbol = desc;
      newSymbol.name  = name;
      if (isGlobal)
        globalSymbols.push_back(newSymbol);
      else
        localSymbols.push_back(newSymbol);

      return true;
    }

    bool program_builder::pop_symbol() {
      scope &block = scopes.back();
      symbol back = symbols.back();
      symbols.pop_back();
      block.symbols.pop_back();
      return true;
    }

    bool program_builder::push_fn_parameter(std::string_view const& identifier, size_t typeIndex, symbol_flags const & flags) {
      symbol symbol;
      symbol.flags       = flags | symbol_flags::const_ | symbol_flags::fn_parameter;
      symbol.type_index  = typeIndex;
      symbol.identifier  = identifier;

      scope &block = scopes.back();
      block.stackSize += get_type_size(typeIndex);

      // Variable starts at the bottom of the stack (so frame pointer - frame size)
      // Variable ends at (frame pointer - frame size + variable size)
      // We offset from frame pointer as it is static during a scope/call. Stack pointer is always moving.
      symbol.address = stack_frame_offset{ block.stackSize };

      return push_symbol(identifier, symbol);
    }

    bool program_builder::push_variable(std::string_view const& identifier, size_t typeIndex, symbol_flags const & flags) {
      auto const& type = types[typeIndex];
      vm::instruction alloc;
      alloc.code = vm::op_code::alloc_stack;
      alloc.alloc_stack.bytes = (uint32_t)get_type_size(type);
      add_instruction(alloc);

      symbol symbol;
      symbol.flags       = flags;
      symbol.type_index  = typeIndex;
      symbol.identifier  = identifier;

      scope &block = scopes.back();
      block.stackSize += get_type_size(type);
      symbol.address = stack_frame_offset{ block.stackSize };

      return push_symbol(identifier, symbol);
    }

    bool program_builder::push_variable(std::string_view const& identifier, std::string_view const & typeName, symbol_flags const & flags) {
      return push_variable(identifier, get_type_index(typeName), flags);
    }

    void program_builder::pop_variable() {
      scope &block = scopes.back();
      symbol var = symbols.back();

      // TODO: Destroy instance by calling destroy function
      // destroy();

      vm::instruction free;
      free.code = vm::op_code::free_stack;
      free.free_stack.bytes = (uint32_t)get_type_size(types[var.type_index]);
      add_instruction(free);

      block.stackSize -= free.free_stack.bytes;
    }

    void program_builder::push_expression_result(expression_result result) {
      results.push_back(result);
    }

    vm::register_index program_builder::pin_register() {
      return registers.pin();
    }

    vm::register_index program_builder::pin_symbol(symbol const& symbol) {
      const size_t sz = get_type_size(types[symbol.type_index]);
      if (sz > sizeof(vm::register_value)) {
        // Error: Cannot pin `symbol` to register. Too large.
      }

      if (symbol.address.has_value()) {
        return pin_address(symbol.address.value(), sz);
      }
      else {
        return pin_relocation(symbol.identifier, sz);
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
      op.load_offset.offset = -offset;
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
        return pin_address(value.address.value(), get_type_size(value.type_index.value()));
      }
      else if (value.symbol_index.has_value()) {
        return pin_symbol(symbols[value.symbol_index.value()]);
      }
      return registers.pin();
    }

    vm::register_index program_builder::pin_address_of(symbol const & symbol) {
      if (symbol.address.has_value()) {
        vm::register_index dst = pin_register();
        set(dst, symbol.address.value());
        return dst;
      }
      else {
        vm::register_index dst = pin_register();
        set(dst, program_address{ 0 });
        uint64_t offset = (uint8_t*)&code.back().set.val - (uint8_t*)code.data();
        relocations[symbol.identifier].push_back(offset);
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
      relocations[identifier].push_back(offset);
      return idx;
    }

    void program_builder::release_register(vm::register_index reg) {
      return registers.release(reg);
    }

    program_builder::expression_result program_builder::pop_expression_result() {
      expression_result ret = results.back();
      results.pop_back();
      return ret;
    }

    //uint64_t program_builder::evaluate_address(uint64_t offset) {
    //  return scopes.back().stackSize - offset;
    //}

    //uint64_t program_builder::evaluate_stack_address(uint64_t offset) {
    //  return scopes.back().stackSize - offset;
    //}

    //uint64_t program_builder::evaluate_address(symbol_desc const & symbol) {
    //  uint64_t offset = std::get<scope_stack_offset>(symbol.address.value()).offset;
    //  return scopes[symbol.scope_index].stackSize - offset;
    //}
    
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
      set(dst, -addr.offset);

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
      instr.store_offset.offset = -addr.offset;
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

    bool program_builder::store(vm::register_index src, symbol const & symbol)
    {
      if (!store(src, symbol.address.value_or(program_address{ 0 }), (uint8_t)get_type_size(types[symbol.type_index])))
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

      relocations[symbol.identifier].push_back((uint8_t*)pAddr - (uint8_t*)code.data());
      return false;
    }

    bool program_builder::store(vm::register_index src, expression_result const& result)
    {
      if (result.symbol_index.has_value()) {
        return store(src, symbols[result.symbol_index.value()]);
      }
      else if (result.type_index.has_value()) {
        if (result.address.has_value()) {
          return store(src, result.address.value(), get_type_size(result.type_index.value()));
        }
        else if (result.constant.has_value()) {
          return store(src, result.constant.value(), get_type_size(result.type_index.value()));
        }
      }

      return false;
    }

    void program_builder::add_instruction(vm::instruction inst) {
      code.push_back(inst);
    }

    program program_builder::binary() const {
      program::header header;

      std::vector<program::symbol_table_entry> publicSymbols;
      std::vector<program::symbol_table_entry> externSymbols;

      std::vector<uint8_t> symbolNames;
      std::vector<uint8_t> symbolData;

      for (auto& symbol : symbols) {
        program::symbol_table_entry item;
        item.name_address = symbolNames.size();
        item.data_address = symbolData.size();

        for (char c : symbol.identifier)
          symbolNames.push_back(c);
        symbolNames.push_back('\0');

        bool isExtern  = (symbol.flags & symbol_flags::extern_) == symbol_flags::extern_;
        bool isAddress = is_function(symbol.type_index) || isExtern;

        if (isExtern)
          externSymbols.push_back(item);
        else
          publicSymbols.push_back(item);
      }

      std::vector<uint8_t> data;
      data.push_back();
    }

    std::optional<std::string> get_type_name(ast const& ast, size_t statement) {
      if (ast.is<expr::type_name>(statement)) {
        return std::string(ast.get<expr::type_name>(statement).name);
      }

      if (ast.is<expr::type_modifier>(statement)) {
        auto& modifier = ast.get<expr::type_modifier>(statement);
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

        return ret + ")=>" + returnName.value();
      }

      return std::nullopt;
    }
  }
}
