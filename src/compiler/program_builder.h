#pragma once

#include "types.h"
#include "ast.h"
#include "ast/builtins.h"

namespace adder {
  namespace compiler {
    struct stack_frame_offset {
      int64_t offset = 0;
    };

    struct program_address {
      uint64_t addr = 0;
    };

    using address_desc = std::variant<stack_frame_offset, program_address>;

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

    struct program_builder {
      program_builder();

      struct symbol_desc {
        /// Symbol type information.
        size_t type_index;
        /// Scope that this symbol was declared in.
        int64_t scope_index = 0;
        /// Identifier of the symbol
        std::string_view identifier;
        /// Flags for the symbol.
        symbol_flags flags = symbol_flags::none;
        /// Address of the symbol within the program, if known.
        std::optional<address_desc> address;
      };
      std::vector<symbol_desc> symbols;

      struct scope {
        std::vector<size_t> symbols;

        int64_t stackSize = 0;
      };

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

      struct Registers {
        std::vector<vm::register_index> free;
        vm::register_index next = 0;

        vm::register_index pin();
        void release(vm::register_index idx);
      } registers;

      struct relocation {
        std::string_view name;
      };

      std::vector<vm::instruction>    code;
      std::vector<scope>              scopes;
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

      size_t get_type_index(std::string_view const& name) const;
      type const * get_type(std::string_view const& name) const;
      size_t get_type_size(type_const const & desc) const;
      size_t get_type_size(type_reference const & desc) const;
      size_t get_type_size(type_primitive const & desc) const;
      size_t get_type_size(type_class const & desc) const;
      size_t get_type_size(type_function const & desc) const;
      size_t get_type_size(type const & type) const;
      size_t get_type_size(size_t const & typeIndex) const;

      std::optional<size_t> find_symbol_index(std::string_view const& identifier) const;
      symbol_desc const * find_symbol(std::string_view const& identifier) const;
      std::optional<size_t> find_unnamed_initializer(size_t receiverTypeIndex, size_t initializerTypeIndex) const;
      bool add_type(type const & desc);
      bool add_function_type(ast const & tree, expr::function_declaration const & decl, std::optional<size_t> id);
      bool push_scope();
      bool pop_scope();

      bool push_symbol(std::string_view const& identifier, symbol_desc const& desc);
      bool pop_symbol();
      bool push_fn_parameter(std::string_view const& identifier, size_t typeIndex, symbol_flags const & flags);
      bool push_variable(std::string_view const& identifier, size_t typeIndex, symbol_flags const & flags);
      bool push_variable(std::string_view const& identifier, std::string_view const & type_name, symbol_flags const & flags);
      void pop_variable();
      void push_expression_result(expression_result result);

      vm::register_index pin_register();
      vm::register_index pin_symbol(symbol_desc const& symbol);
      vm::register_index pin_constant(vm::register_value value);
      vm::register_index pin_address(program_address address, size_t size);
      vm::register_index pin_address(stack_frame_offset stack, size_t size);
      vm::register_index pin_address(address_desc address, size_t size);
      vm::register_index pin_address(uint64_t address, size_t size);
      vm::register_index pin_stack_frame_offset(int64_t offset, size_t size);
      vm::register_index pin_result(expression_result const & value);
      vm::register_index pin_relocation(std::string_view identifier, size_t size);

      void release_register(vm::register_index reg);

      expression_result pop_expression_result();
      // uint64_t evaluate_address(uint64_t offset);
      // uint64_t evaluate_stack_address(uint64_t offset);
      // uint64_t evaluate_address(symbol_desc const & symbol);

      bool store(vm::register_index src, vm::register_index dst, uint8_t sz);
      bool store(vm::register_index src, program_address const & addr, uint8_t sz);
      bool store(vm::register_index src, stack_frame_offset const & addr, uint8_t sz);
      bool store(vm::register_index src, address_desc const & addr, uint8_t sz);
      bool store(vm::register_index src, symbol_desc const & symbol);
      void add_instruction(vm::instruction inst);

      /// Convert the program to a binary
      // program binary() const {
      //   return {};
      // }
    };
  }
}
