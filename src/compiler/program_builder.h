#pragma once

#include "types.h"
#include "ast.h"
#include "ast/builtins.h"

namespace adder {
  namespace compiler {
    struct stack_offset {
      uint64_t offset = 0;
    };

    struct program_offset {
      uint64_t offset = 0;
    };

    using address_desc = std::variant<stack_offset, program_offset>;

    // Symbols are prefixed with the symbol type, e.g. fn, init, class, var.
    // Symbols components are separated by ':'.
    // Components in a symbol are determined by the type.
    //
    // e.g.,
    //
    //  var:type:name
    //  fn:name:arg0,arg1,...,argN
    //  init:type:name:arg0,arg1,...,argN
    //  method:type:name:arg0,arg1,...,argN
    //
    struct program_builder {
      program_builder() {
        scopes.push_back({});
      }

      struct symbol_desc {
        struct function_def {
          bool allowInline = false;
          size_t function_id; ///< Expression that contains the function definition. Used to generate inline code
        };
        /// Symbol type information.
        size_t type_index;
        /// Scope that this symbol was declared in.
        int64_t scope_index = 0;
        /// Identifier of the symbol
        std::string_view identifier;
        /// Flags for the symbol.
        symbol_flags flags = symbol_flags::none;
        /// Code associated with the symbol (e.g. function body)
        std::optional<function_def> function;
        /// Address of the symbol within the program, if known.
        std::optional<address_desc> address;
      };
      std::vector<symbol_desc> symbols;

      struct scope {
        std::vector<size_t> symbols;

        uint64_t stackSize = 0;
      };

      // struct relocation {
      //   uint64_t offset; ///< Where the resolve symbol is written to.
      //   uint64_t symbol; ///< The index of the symbol in the program.
      // };

      struct expression_result {
        ///< Constant value evaluated
        std::optional<vm::register_value> constant;
        ///< Address of the value
        std::optional<uint64_t> stack_address;
        ///< Index of the symbol
        std::optional<size_t> symbol_index;
        ///< Type of the value
        std::optional<size_t> type_index;
      };

      struct Registers {
        std::vector<vm::register_index> free;
        vm::register_index next = 0;

        vm::register_index pin() {
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

        void release(vm::register_index idx) {
          free.push_back(idx);
        }
      } registers;
      std::vector<vm::instruction>    code;
      // std::vector<relocation>         relocations;
      std::vector<scope>              scopes;
      std::vector<expression_result>  results;

      std::vector<type> types = {
        { "void",    { type_primitive::_void } },
        { "int8",    { type_primitive::int8 } },
        { "int16",   { type_primitive::int16 } },
        { "int32",   { type_primitive::int32 } },
        { "int64",   { type_primitive::int64 } },
        { "uint8",   { type_primitive::int8 } },
        { "uint16",  { type_primitive::uint16 } },
        { "uint32",  { type_primitive::uint32 } },
        { "uint64",  { type_primitive::uint64 } },
        { "float32", { type_primitive::float32  }},
        { "float64", { type_primitive::float64 } },
        { "bool",    { type_primitive::bool_ } }
      };

      size_t get_type_index(std::string_view const& name) const {
        auto it = std::find_if(types.begin(), types.end(), [&](type const& t) { return t.identifier == name; });
        return it - types.begin();
      }

      type const * get_type(std::string_view const& name) const {
        auto it = std::find_if(types.begin(), types.end(), [&](type const& t) { return t.identifier == name; });
        if (it == types.end())
          return nullptr;
        return &(*it);
      }

      std::optional<size_t> find_symbol_index(std::string_view const& identifier) const {
        auto it = std::find_if(
          symbols.rbegin(),
          symbols.rend(),
          [&](const symbol_desc& symbol) { return symbol.identifier == identifier; });
        return it == symbols.rend() ? -1 : &(*it) - symbols.data();
      }

      symbol_desc const * find_symbol(std::string_view const& identifier) const {
        std::optional<size_t> idx = find_symbol_index(identifier);

        return idx.has_value() ? &symbols[idx.value()] : nullptr;
      }

      std::optional<size_t> find_unnamed_initializer(size_t receiverTypeIndex, size_t initializerTypeIndex) const {
        std::string_view signature = adder::format(
          "init:%.*s::%.*s",
          types[receiverTypeIndex].identifier.length(), types[receiverTypeIndex].identifier.data(),
          types[initializerTypeIndex].identifier.length(), types[initializerTypeIndex].identifier.data()
        );

        return find_symbol_index(signature);
      }

      bool add_type(type const & desc) {
        if (get_type(desc.identifier) != nullptr)
          return false;
        types.push_back(desc);
        return true;
      }

      bool push_scope() {
        scopes.emplace_back();
      }

      bool pop_scope() {
        scope &block = scopes.back();
        while (block.symbols.size() > 0)
        {
          block.symbols.pop_back();
          symbols.pop_back();
        }
        scopes.pop_back();
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

      bool push_symbol(std::string_view const& identifier, symbol_desc const& desc) {
        scope &block = scopes.back();
        block.symbols.push_back(symbols.size() - 1);

        symbol_desc newSymbol = desc;
        newSymbol.identifier  = identifier;
        newSymbol.scope_index = scopes.size() - 1;
        symbols.push_back(newSymbol);
        return true;
      }

      bool pop_symbol() {
        scope &block = scopes.back();
        symbol_desc back = symbols.back();
        symbols.pop_back();
        block.symbols.pop_back();
        return true;
      }

      bool push_variable(std::string_view const& identifier, size_t typeIndex, symbol_flags const & flags) {
        auto const& type = types[typeIndex];
        vm::instruction alloc;
        alloc.code = vm::op_code::alloc_stack;
        alloc.alloc_stack.bytes = (uint32_t)type_size(type);
        add_instruction(alloc);

        symbol_desc symbol;
        symbol.flags       = flags;
        symbol.type_index  = typeIndex;
        symbol.identifier  = identifier;

        scope &block = scopes.back();
        symbol.address = stack_offset{ block.stackSize };
        block.stackSize += type_size(type);

        return push_symbol(identifier, symbol);
      }

      bool push_variable(std::string_view const& identifier, std::string_view const & type_name, symbol_flags const & flags) {
        return push_variable(identifier, get_type_index(type_name), flags);
      }

      void pop_variable() {
        scope &block = scopes.back();
        symbol_desc var = symbols.back();

        // TODO: Destroy instance
        // destroy();

        vm::instruction free;
        free.code = vm::op_code::free_stack;
        free.free_stack.bytes = (uint32_t)type_size(types[var.type_index]);
        add_instruction(free);

        block.stackSize -= free.free_stack.bytes;
      }

      void push_expression_result(expression_result result) {
        results.push_back(result);
      }

      vm::register_index pin_register() {
        return registers.pin();
      }

      vm::register_index pin_symbol(symbol_desc const& symbol) {
        const size_t sz = type_size(types[symbol.type_index]);
        if (sz > sizeof(vm::register_value)) {
          // Error: Cannot pin `symbol` to register. Too large.
        }

        if (symbol.address.has_value())
          pin_address(symbol.address.value(), sz);
      }

      vm::register_index pin_constant(vm::register_value value) {
        vm::register_index idx = registers.pin();
        vm::instruction op;
        op.code = vm::op_code::set;
        op.set.val = value;
        op.set.dst = idx;
        add_instruction(op);
        return idx;
      }

      vm::register_index pin_address(program_offset address, size_t size) {
        return pin_address(address.offset, size);
      }

      vm::register_index pin_address(stack_offset stack, size_t size) {
        return pin_address(evaluate_stack_address(stack.offset), size);
      }

      vm::register_index pin_address(address_desc address, size_t size) {
        return std::visit([=](auto&& o) { return pin_address(o, size); }, address);
      }

      vm::register_index pin_address(uint64_t address, size_t size) {
        vm::register_index idx = registers.pin();
        vm::instruction op;
        op.code = vm::op_code::load_addr;
        op.load_addr.addr = address;
        op.load_addr.size = (uint8_t)size;
        op.load_addr.dst = idx;
        add_instruction(op);
        return idx;
      }

      vm::register_index pin_result(expression_result value, size_t size) {
        if (value.constant.has_value()) {
          return pin_constant(value.constant.value());
        }
        else if (value.stack_address.has_value()) {
          return pin_address(value.stack_address.value(), size);
        }
        else {
          // src = program->load_relocated_address(value.symbol_name);
        }
        return registers.pin();
      }

      void release_register(vm::register_index reg) {
        return registers.release(reg);
      }

      expression_result pop_expression_result() {
        expression_result ret = results.back();
        results.pop_back();
        return ret;
      }

      uint64_t evaluate_address(uint64_t offset) {
        return scopes.back().stackSize - offset;
      }

      uint64_t evaluate_stack_address(uint64_t offset) {
        return scopes.back().stackSize - offset;
      }

      uint64_t evaluate_address(symbol_desc const & symbol) {
        uint64_t offset = std::get<stack_offset>(symbol.address.value()).offset;
        return scopes[symbol.scope_index].stackSize - offset;
      }

      void add_instruction(vm::instruction inst) {
        code.push_back(inst);
      }

      /// Convert the program to a binary
      // program binary() const {
      //   return {};
      // }
    };
  }
}
