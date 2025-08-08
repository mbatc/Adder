#pragma once

#include "types.h"
#include "ast.h"
#include "ast/builtins.h"
#include "../containers/pool.h"

namespace adder {
  namespace compiler {
    struct program;

    struct stack_frame_offset {
      int64_t offset = 0;
    };

    struct program_address {
      uint64_t addr = 0;
    };

    using address_desc = std::variant<stack_frame_offset, program_address>;

    std::optional<std::string> get_type_name(ast const & ast, size_t statement);

    // Symbols are prefixed with the symbol type, e.g. fn, init, class, var.
    // Symbols components are separated by ':'.
    // Components in a symbol are determined by the type.
    //
    // e.g.,
    //
    //  var:type:name
    //  fn:arg0,arg1,...,argN:name
    //  init:[ref]type,arg0,arg1,...,argN:name
    //  method:[ref]type,arg0,arg1,...,argN:name

    struct program_builder {
      program_builder();

      struct Registers {
        std::vector<vm::register_index> free;
        vm::register_index next = 0;

        vm::register_index pin();
        void release(vm::register_index idx);
      } registers;

      struct symbol {
        /// Full symbol name for linking.
        std::string name;
        /// Symbol type information.
        size_t type_index;
        /// Flags for the symbol.
        symbol_flags flags = symbol_flags::none;
        /// Address of the symbol within the program, if known.
        std::optional<address_desc> address;

        struct function_block {
          size_t instruction_offset = 0;
          size_t instruction_count = 0;
        };

        /// Function instruction offset
        std::optional<function_block> function;
      };

      struct identifier {
        /// Name of the identifier
        std::string name;
        /// Index of the program resource
        std::optional<size_t> symbol_index;
      };

      /// Symbols in the program.
      adder::pool<symbol> symbols;
      /// Stack of prefixes for symbols
      std::vector<std::string> symbolPrefix;
      /// Instructions in the program.
      std::vector<vm::instruction> code;

      struct scope {
        /// Visibile identifiers.
        std::vector<size_t> localSymbols;
        /// Visibile identifiers.
        std::vector<identifier> identifiers;
        /// Size of the stack frame.
        int64_t stackSize = 0;
      };
      std::vector<scope> scopes;

      // struct relocation {
      //   uint64_t offset; ///< Where the resolve symbol is written to.
      //   uint64_t symbol; ///< The index of the symbol in the program.
      // };

      struct expression_result {
        ///< Constant value evaluated
        std::optional<vm::register_value> constant;
        ///< Address of the value
        std::optional<address_desc> address;
        ///< Index of the symbol
        std::optional<size_t> symbol_index;
        ///< Type of the value
        std::optional<size_t> type_index;
      };
      std::vector<expression_result>  results;

      /// Identifiers whose location needs to be resolved.
      /// [identifier] -> list of offsets into instructions. Offset is in bytes.
      std::map<std::string_view, std::vector<uint64_t>> relocations;

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

      size_t get_type_index(std::string_view const & name) const;
      type const * get_type(std::string_view const & name) const;
      
      size_t get_type_index(ast const & tree, size_t statement) const;
      type const * get_type(ast const & tree, size_t statement) const;

      std::optional<size_t> unwrap_type(size_t const & type) const;
      bool is_reference_of(std::optional<size_t> const & reference, std::optional<size_t> const & baseType) const;
      bool is_reference(std::optional<size_t> const & type) const;
      bool is_const(std::optional<size_t> const & type) const;
      bool is_function(std::optional<size_t> const & type) const;

      size_t get_type_size(type_modifier const & desc) const;
      size_t get_type_size(type_primitive const & desc) const;
      size_t get_type_size(type_class const & desc) const;
      size_t get_type_size(type_function const & desc) const;
      size_t get_type_size(type_function_decl const & desc) const;
      size_t get_type_size(type const & type) const;
      size_t get_type_size(size_t const & typeIndex) const;

      std::optional<size_t> find_symbol_index(std::string_view const& identifier) const;
      symbol const * find_symbol(std::string_view const& identifier) const;
      std::optional<size_t> lookup_identifier_symbol_index(std::string_view const& identifier) const;
      symbol const * lookup_identifier_symbol(std::string_view const& identifier) const;
      std::optional<size_t> find_unnamed_initializer(size_t receiverTypeIndex, size_t initializerTypeIndex) const;

      size_t add_type(type const & desc);
      size_t add_function_type(ast const & tree, expr::function_declaration const & decl, std::optional<size_t> id);
      bool push_scope();
      bool pop_scope();
      void push_symbol_prefix(std::string const & name);
      void pop_symbol_prefix();

      void call(symbol const & symbol);
      void jump_to(symbol const & symbol);
      void jump_to(program_address const & address);
      void jump_indirect(address_desc const & addr);
      void jump_indirect(vm::register_index const & address);
      void push_return_pointer();
      void pop_return_pointer();

      std::optional<size_t> push_symbol(symbol desc);
      bool push_fn_parameter(std::string_view const& identifier, size_t typeIndex, symbol_flags const & flags);
      bool push_variable(std::string_view const& identifier, size_t typeIndex, symbol_flags const & flags);
      bool push_variable(std::string_view const& identifier, std::string_view const & type_name, symbol_flags const & flags);
      bool push_identifier(std::string_view const& identifier, symbol const & symbol);
      void push_expression_result(expression_result result);

      /// Push a register value to the stack
      void push(vm::register_index const & src);
      /// Pop a register value from the stack
      void pop(vm::register_index const & dst);

      vm::register_index pin_register();
      vm::register_index pin_symbol(symbol const& symbol);
      vm::register_index pin_constant(vm::register_value value);
      vm::register_index pin_address(program_address address, size_t size);
      vm::register_index pin_address(stack_frame_offset stack, size_t size);
      vm::register_index pin_address(address_desc address, size_t size);
      vm::register_index pin_address(uint64_t address, size_t size);
      vm::register_index pin_stack_frame_offset(int64_t offset, size_t size);
      vm::register_index pin_result(expression_result const & value);
      vm::register_index pin_address_of(symbol const& symbol);
      vm::register_index pin_address_of(expression_result const & result);
      vm::register_index pin_relocation(std::string_view identifier, size_t size);

      void release_register(vm::register_index reg);

      expression_result pop_expression_result();

      void move(vm::register_index dst, vm::register_index src);

      void set(vm::register_index dst, vm::register_value value);
      void set(vm::register_index dst, program_address const& addr);
      void set(vm::register_index dst, stack_frame_offset const& addr);
      void set(vm::register_index dst, address_desc const& addr);

      // bool load(vm::register_index dst, vm::register_value value);
      // bool load(vm::register_index dst, program_address const & addr);
      // bool load(vm::register_index dst, stack_frame_offset const & addr);
      // bool load(vm::register_index dst, address_desc const & addr);

      bool store(vm::register_index src, vm::register_index dst, uint8_t sz);
      bool store(vm::register_index src, program_address const & addr, uint8_t sz);
      bool store(vm::register_index src, stack_frame_offset const & addr, uint8_t sz);
      bool store(vm::register_index src, address_desc const & addr, uint8_t sz);
      bool store(vm::register_index src, symbol const & symbol);
      bool store(vm::register_index src, expression_result const & result);
      void add_instruction(vm::instruction inst);

      /// Convert the program to a binary
      program binary() const;
    };
  }
}
