#pragma once

#include "types.h"
#include "ast.h"
#include "ast/builtins.h"
#include "../containers/pool.h"

#include <functional>

namespace adder {
  struct program;

  namespace compiler {
    // struct stack_frame_offset {
    //   int64_t offset = 0;
    // };
    // 
    // struct program_address {
    //   uint64_t addr = 0;
    // };
    // 
    // using address_desc = std::variant<stack_frame_offset, program_address>;

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
        /// Parent scope of this statement is 
        size_t parent_scope_id;
        /// Scope created by this statement
        std::optional<size_t> scope_id;
        /// Symbol the statement is associated with (if any)
        std::optional<size_t> symbol_index;
        /// Type that this statement is associated with (if any)
        std::optional<size_t> type_id;
        /// How many bytes of temp storage are required for this statement.
        size_t temp_storage = 0;
      };
      std::vector<statement_meta> statement_info;

      struct symbol {
        std::string name;
        std::string full_identifier;
        size_t      type        = 0;

        /// Flags for the symbol
        symbol_flags flags = symbol_flags::none;

        /// Stack frame offset for local variables
        std::optional<uint64_t> stack_offset;

        /// Static address for static/global.
        /// Cannot be resolved until program data area has been compiled.
        std::optional<uint64_t> global_address;

        /// Statement that produced this symbol
        size_t statement_id = 0;

        /// Scope that declared this symbol
        size_t scope_id = 0;

        /// Function declaration associated with this symbol
        std::optional<size_t> function_index;

        bool is_parameter() const { return (flags & symbol_flags::fn_parameter) != symbol_flags::none; }
        bool is_static()    const { return (flags & symbol_flags::static_) != symbol_flags::none; }
        bool is_function()  const { return (flags & symbol_flags::function) != symbol_flags::none; }
        bool is_local()     const { return scope_id != 0; };
      };
      std::vector<symbol> symbols;

      struct scope {
        std::string           prefix;
        std::vector<size_t>   symbols;
        /// The direct parent of this scope.
        std::optional<size_t> parent;
        /// Scope ID of the function that contains this scope.
        /// If nullopt, this is the root scope of a function.
        std::optional<size_t> function_scope;

        /// Upper bound of space allocated for this function.
        /// Includes variables in all nested scopes.
        size_t max_stack_size = 0;
        /// Used when generating scope metadata to track the required stack space at the current scope.
        size_t stack_size_temp = 0;
        /// Max size of temporary space needed to evaluate an expression.
        /// A function scope must allocate the max_stack_size + max_temp_size for the stack frame.
        size_t max_temp_size = 0;
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

      std::optional<size_t> add_symbol(symbol const & s);
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
        // struct State {
        //   vm::register_index index;
        //   std::optional<uint64_t> address;
        //   std::optional<uint64_t> value;
        //   std::optional<uint64_t> constant;
        // };
        std::vector<vm::register_index> free;
        vm::register_index next = 0;

        vm::register_index pin();
        void release(vm::register_index idx);
      } registers;

      struct function {
        size_t symbol;
        size_t scope_id;

        size_t args_size = 0; ///< Size of the function parameters.
        size_t stack_size = 0;
        size_t temp_symbols_size = 0;

        std::vector<vm::instruction> instructions;
      };
      std::vector<size_t>   function_stack;
      std::vector<function> functions;

      struct value {
        std::optional<std::string> identifier;
        /// Constant value evaluated
        std::optional<vm::register_value> constant;
        /// Index of the symbol
        std::optional<size_t> stack_frame_offset;
        /// Index of the symbol
        std::optional<size_t> symbol_index;
        /// Type of the value
        std::optional<size_t> type_index;
        /// Base address offset to the value (if applicable)
        size_t address_offset;
      };
      std::vector<value> results;

      struct scope {
        size_t stack_bytes = 0;

        std::vector<value> variables;
      };
      std::vector<scope> scopes;

      void push_result(value r);
      std::optional<value> pop_result();

      struct relocation {
        std::string_view symbol;
        uint64_t         offset;
        size_t           function_id;
      };

      /// Identifiers whose location needs to be resolved.
      /// [identifier] -> list of offsets into instructions. Offset is in bytes
      std::vector<relocation> relocations;

      bool begin_function(size_t symbol);
      void end_function();
      function& current_function();

      bool begin_scope();
      bool end_scope();

      void push_return_handler(const std::function<void(program_builder*)>& handler);
      void pop_return_handler();
      void return_with_return_handler();
      std::vector<std::function<void(program_builder*)>> return_handler_stack;

      /// Get a value that describes the return value.
      value get_return_value(size_t typeIndex) const;

      /// Add an identifier to the current scope
      void add_variable(program_builder::value const & val);

      /// Find a symbol by identifier. Searches from the inner most scope outwards.
      std::optional<value> find_value_by_identifier(std::string_view const & name) const;
      std::optional<value> find_value_by_identifier(std::string_view const & name, size_t scopeIndex) const;

      /// Allocate space for a return value and push an expression_result to the result stack.
      program_builder::value allocate_value(size_t typeIndex);

      size_t current_scope_id() const;
      size_t current_scope_stack_size() const;

      // expression_result alloc_temporary_value(size_t typeIndex);

      void add_relocation(std::string_view const& symbol, uint64_t offset);

      void call(program_metadata::symbol const & symbol);
      void call(uint64_t address);
      void call_indirect(vm::register_index const & symbol);
      void ret();

      void jump_to(program_metadata::symbol const & symbol);
      void jump_to(uint64_t address);
      void jump_indirect(vm::register_index const & address);
      void jump_relative(int64_t offset);

      void push_return_pointer();
      void push_frame_pointer();
      void pop_return_pointer();
      void pop_frame_pointer();

      // bool push_return_value_alias(std::string_view const& identifier, size_t typeIndex, symbol_flags const & flags);
      // bool push_fn_parameter(std::string_view const& identifier, size_t typeIndex, symbol_flags const & flags);
      // bool push_variable(std::string_view const& identifier, size_t typeIndex, symbol_flags const & flags);
      // bool push_variable(std::string_view const& identifier, std::string_view const & type_name, symbol_flags const & flags);
      // bool push_identifier(std::string_view const& identifier, program_metadata::symbol const & symbol);
      // void push_expression_result(expression_result result);

      void alloc_stack(size_t bytes);
      void free_stack(size_t bytes);

      /// Push a register value to the stack
      void push(vm::register_index const & src);
      /// Pop a register value from the stack
      void pop(vm::register_index const & dst);

      vm::register_index pin_register();
      vm::register_index load_stack_frame_offset(int64_t offset, size_t size);
      // vm::register_index load_result(expression_result const & value);
      // vm::register_index load_address_of(expression_result const & result);

      vm::register_index load_constant(vm::register_value value);
      vm::register_index load_value_of(program_metadata::symbol const & symbol);
      vm::register_index load_value_of(uint64_t address, size_t size);
      vm::register_index load_value_of(program_builder::value value);
      vm::register_index load_address_of(program_metadata::symbol const & symbol);
      vm::register_index load_address_of(std::string_view identifier, size_t size);
      vm::register_index load_address_of(program_builder::value value);
      void release_register(vm::register_index reg);

      // expression_result pop_expression_result();
      void load(vm::register_index dst, vm::register_index address, size_t size, int64_t offset);
      void load(vm::register_index dst, vm::register_index address, size_t size);
      void move(vm::register_index dst, vm::register_index src);
      void set(vm::register_index dst, vm::register_value value);
      bool store(vm::register_index src, vm::register_index dst, uint8_t sz);
      void addi(vm::register_index dst, vm::register_index a, vm::register_index b);
      void addf(vm::register_index dst, vm::register_index a, vm::register_index b);
      void subi(vm::register_index dst, vm::register_index a, vm::register_index b);
      void subf(vm::register_index dst, vm::register_index a, vm::register_index b);
      void divi(vm::register_index dst, vm::register_index a, vm::register_index b);
      void divf(vm::register_index dst, vm::register_index a, vm::register_index b);
      void muli(vm::register_index dst, vm::register_index a, vm::register_index b);
      void mulf(vm::register_index dst, vm::register_index a, vm::register_index b);

      void add_instruction(vm::instruction inst);

      /// Convert the program to a binary
      program binary() const;
    };
  }
}
