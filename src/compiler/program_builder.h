#pragma once

#include "types.h"
#include "ast.h"
#include "ast/builtins.h"
#include "../containers/pool.h"

#include <functional>

namespace adder {
  struct program;

  namespace compiler {
    struct stack_frame_offset {
      int64_t offset = 0;
    };

    struct program_address {
      uint64_t addr = 0;
    };

    using address_desc = std::variant<stack_frame_offset, program_address>;

    std::optional<std::string> get_type_name(ast const & ast, size_t statement);
    std::optional<std::string> get_symbol_name(ast const & ast, size_t statement, std::string_view const & identifier);
    std::string get_symbol_name(std::string_view const & typeName, std::string_view const & identifier);

    struct program_metadata {
      size_t static_storage_size = 0;

      std::vector<type> types = {
        { (std::string)get_primitive_type_name(type_primitive::void_),   { type_primitive::void_ } },
        { (std::string)get_primitive_type_name(type_primitive::int8),    { type_primitive::int8 } },
        { (std::string)get_primitive_type_name(type_primitive::int16),   { type_primitive::int16 } },
        { (std::string)get_primitive_type_name(type_primitive::int32),   { type_primitive::int32 } },
        { (std::string)get_primitive_type_name(type_primitive::int64),   { type_primitive::int64 } },
        { (std::string)get_primitive_type_name(type_primitive::uint8),   { type_primitive::uint8 } },
        { (std::string)get_primitive_type_name(type_primitive::uint16),  { type_primitive::uint16 } },
        { (std::string)get_primitive_type_name(type_primitive::uint32),  { type_primitive::uint32 } },
        { (std::string)get_primitive_type_name(type_primitive::uint64),  { type_primitive::uint64 } },
        { (std::string)get_primitive_type_name(type_primitive::float32), { type_primitive::float32  }},
        { (std::string)get_primitive_type_name(type_primitive::float64), { type_primitive::float64 } },
        { (std::string)get_primitive_type_name(type_primitive::bool_),   { type_primitive::bool_ } }
      };

      /// Additional metadata for each statement.
      /// TODO: Undecided if this should just be stored in the AST.
      struct statement_meta {
        size_t                scope_id;
        std::optional<size_t> type_id;
      };
      std::vector<statement_meta> statement_info;

      struct symbol {
        std::string name;
        std::string full_identifier;
        size_t      type        = 0;

        /// Flags for the symbol
        symbol_flags flags = symbol_flags::none;

        /// Address (static/global)
        ///   * Stack frame offset for local variables
        ///   * Static address for static/global
        ///   * Ignored for extern
        address_desc address;

        /// Statement that produced this symbol
        size_t statement_id = 0;

        std::optional<size_t> function_index;
      };
      std::vector<symbol> symbols;

      struct scope {
        std::string           prefix;
        std::vector<size_t>   symbols;
        std::optional<size_t> parent;

        /// If a stack frame was pushed with this scope.
        bool is_stack_frame = false;

        size_t symbols_size = 0;
      };
      std::vector<scope> scopes;

      size_t add_type(type const & desc);
      size_t add_function_type(ast const & tree, expr::function_declaration const & decl, std::optional<size_t> id);

      std::optional<size_t> get_type_index(std::string_view const & name) const;
      type const * get_type(std::string_view const & name) const;

      std::optional<size_t> get_type_index(ast const & tree, size_t statement) const;
      type const * get_type(ast const & tree, size_t statement) const;

      std::optional<size_t> unwrap_type   (std::optional<size_t> const & type) const;
      std::optional<size_t> decay_type    (std::optional<size_t> const & type) const;
      std::optional<size_t> return_type_of(std::optional<size_t> const & func) const;

      bool is_reference_of(std::optional<size_t> const & reference, std::optional<size_t> const & baseType) const;
      bool is_reference(std::optional<size_t> const & type) const;
      bool is_const(std::optional<size_t> const & type) const;
      bool is_function_decl(std::optional<size_t> const& type) const;
      bool is_function(std::optional<size_t> const & type) const;
      bool is_integer(std::optional<size_t> const & type) const;
      bool is_float(std::optional<size_t> const & type) const;
      bool is_bool(std::optional<size_t> const & type) const;
      bool is_void(std::optional<size_t> const & type) const;

      bool is_valid_function_overload(std::optional<size_t> const & a, std::optional<size_t> const & b) const;

      size_t get_type_size(type_modifier const & desc) const;
      size_t get_type_size(type_primitive const & desc) const;
      size_t get_type_size(type_class const & desc) const;
      size_t get_type_size(type_function const & desc) const;
      size_t get_type_size(type_function_decl const & desc) const;
      size_t get_type_size(type const & type) const;
      size_t get_type_size(size_t const & typeIndex) const;

      std::optional<size_t> add_symbol(size_t scopeId, symbol const & s);
      std::optional<size_t> search_for_symbol_index(size_t scopeId, std::string_view const & identifier) const;
      std::optional<size_t> search_for_symbol_index(size_t scopeId, std::function<bool(symbol const &)> const & pred) const;
      std::optional<size_t> find_symbol(std::string_view const & fullName) const;
      std::optional<size_t> find_unnamed_initializer(size_t scopeId, size_t receiverTypeIndex, size_t initializerTypeIndex) const;

      std::optional<size_t> get_parent_scope(size_t const & scopeId) const;
    };

    struct program_builder {
      program_builder() {}

      program_metadata meta;

      struct Registers {
        std::vector<vm::register_index> free;
        vm::register_index next = 0;

        vm::register_index pin();
        void release(vm::register_index idx);
      } registers;

      struct function {
        size_t symbol;
        size_t scope_id;

        std::vector<vm::instruction> instructions;
      };
      std::vector<size_t>   function_stack;
      std::vector<function> functions;

      struct expression_result {
        ///< Constant value evaluated
        std::optional<vm::register_value> constant;
        ///< Address of the value
        std::optional<address_desc> address;
        ///< Index of the symbol
        std::optional<size_t> symbol_index;
        ///< Type of the value
        std::optional<size_t> type_index;
        ///< Size of temporary storage allocated for this result
        std::optional<size_t> alloc_size;
      };
      std::vector<expression_result>  results;
      void pop_result();
      void push_result(expression_result r);

      struct relocation {
        std::string_view symbol;
        uint64_t         offset;

      };

      /// Identifiers whose location needs to be resolved.
      /// [identifier] -> list of offsets into instructions. Offset is in bytes
      std::vector<relocation> relocations;
      std::vector<std::vector<relocation>> scratchRelocations;

      struct scope {
        size_t meta_index = 0;
      };

      void begin_function(size_t symbol, size_t scope_id);
      void finish_function();

      bool begin_scope(size_t scopeId);
      bool end_scope();
      size_t current_scope_id() const;

      expression_result alloc_temporary_value(size_t typeIndex);

      void add_relocation(std::string_view const& symbol, uint64_t offset);
      void begin_relocation_group();

      std::pair<size_t, size_t> end_relocation_group();

      void call(program_metadata::symbol const & symbol);
      void call(program_address const & symbol);
      void call_indirect(vm::register_index const & symbol);
      void call_indirect(address_desc const & symbol);
      void ret();

      void jump_to(program_metadata::symbol const & symbol);
      void jump_to(program_address const & address);
      void jump_indirect(address_desc const & addr);
      void jump_indirect(vm::register_index const & address);

      void push_return_pointer();
      void push_frame_pointer();
      void pop_return_pointer();
      void pop_frame_pointer();

      bool push_return_value_alias(std::string_view const& identifier, size_t typeIndex, symbol_flags const & flags);
      bool push_fn_parameter(std::string_view const& identifier, size_t typeIndex, symbol_flags const & flags);
      bool push_variable(std::string_view const& identifier, size_t typeIndex, symbol_flags const & flags);
      bool push_variable(std::string_view const& identifier, std::string_view const & type_name, symbol_flags const & flags);
      bool push_identifier(std::string_view const& identifier, program_metadata::symbol const & symbol);
      void push_expression_result(expression_result result);
      
      /// Push a register value to the stack
      void push(vm::register_index const & src);
      /// Pop a register value from the stack
      void pop(vm::register_index const & dst);

      vm::register_index pin_register();
      vm::register_index pin_symbol(program_metadata::symbol const & symbol);
      vm::register_index pin_constant(vm::register_value value);
      vm::register_index pin_address(program_address address, size_t size);
      vm::register_index pin_address(stack_frame_offset stack, size_t size);
      vm::register_index pin_address(address_desc address, size_t size);
      vm::register_index pin_address(uint64_t address, size_t size);
      vm::register_index pin_stack_frame_offset(int64_t offset, size_t size);
      vm::register_index pin_result(expression_result const & value);
      vm::register_index pin_address_of(program_metadata::symbol const & symbol);
      vm::register_index pin_address_of(expression_result const & result);
      vm::register_index pin_relocation(std::string_view identifier, size_t size);

      void load(vm::register_index dst, vm::register_index address, size_t size, int64_t offset);
      void load(vm::register_index dst, vm::register_index address, size_t size);
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
      bool store(vm::register_index src, program_metadata::symbol const & symbol);
      bool store(vm::register_index src, expression_result const & result);

      void addi(vm::register_index dst, vm::register_index a, vm::register_index b);
      void addf(vm::register_index dst, vm::register_index a, vm::register_index b);

      void add_instruction(vm::instruction inst);

      /// Convert the program to a binary
      program binary() const;
    };
  }
}
