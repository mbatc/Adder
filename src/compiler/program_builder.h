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
        /// Scope associated with this symbol (if any)
        std::optional<size_t> scope_index;
        /// Symbol the statement is associated with (if any)
        std::optional<size_t> symbol_index;
        /// Type that this statement is associated with (if any)
        std::optional<size_t> type_id;
        /// Type of temporary storage needed
        std::optional<size_t> temporary_type;
      };
      std::vector<statement_meta> statement_info;

      struct symbol {
        std::string name;
        std::string full_identifier;
        size_t type = 0;

        /// Flags for the symbol
        symbol_flags flags = symbol_flags::none;
        /// Stack frame offset for local variables
        std::optional<uint64_t> stack_offset;
        /// Static address for static/global.
        /// Cannot be resolved until program data area has been compiled.
        std::optional<uint64_t> global_address;
        /// Statement that produced this symbol.
        /// size_t statement_id = 0;
        /// Scope that declared this symbol.
        size_t scope_id = 0;
        /// ID if the statement that declared this symbol
        std::optional<size_t> declaration_id;
        /// Function declaration root scope id.
        std::optional<size_t> function_root_scope_id;
        /// Function declaration associated with this symbol.
        std::optional<size_t> function_index;

        bool is_parameter() const { return (flags & symbol_flags::fn_parameter) != symbol_flags::none; }
        bool is_static()    const { return (flags & symbol_flags::static_) != symbol_flags::none; }
        bool is_function()  const { return (flags & symbol_flags::function) != symbol_flags::none; }
        bool is_global()     const { return scope_id == 0; };
        bool has_local_storage() const { return !is_global() && !is_static(); }
      };
      std::vector<symbol> symbols;

      struct scope {
        /// Unique symbol prefix for this scope.
        std::string           prefix;
        /// Symbols declared in this scope
        std::vector<size_t>   symbols;
        /// The direct parent of this scope.
        std::optional<size_t> parent;
        /// Next sibling of this scope.
        std::optional<size_t> sibling;
        /// First child of this scope.
        std::optional<size_t> first_child;
        /// Scope ID of the function that contains this scope.
        /// If nullopt, this is the root scope of a function.
        std::optional<size_t> parent_function_scope;
        /// Upper bound of space allocated for this function.
        /// Includes variables in all nested scopes.
        size_t max_stack_size = 0;
        /// Max size of temporary space needed to evaluate an expression.
        /// A function scope must allocate the max_stack_size + max_temp_size for the stack frame.
        size_t max_temp_size = 0;
      };
      std::vector<scope> scopes;

      size_t new_scope(size_t parent);
      void for_each_child_scope(size_t root, std::function<void(size_t)> const& cb);

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
      size_t get_symbol_size(size_t const & symbolIndex) const;
      size_t get_symbol_type(size_t const & symbolIndex) const;
      std::optional<size_t> search_for_symbol_index(size_t scopeId, std::string_view const & identifier) const;
      std::optional<size_t> search_for_symbol_index(size_t scopeId, std::function<bool(symbol const &)> const & pred) const;
      std::optional<size_t> search_for_callable_symbol_index(size_t scopeId, std::string_view const & identifier, ast const& ast, std::optional<size_t> const & paramList) const;
      std::optional<size_t> get_parameter_list_score(size_t scopeId, size_t funcType, ast const & ast, std::optional<size_t> const & paramList) const;

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

      enum class value_flags {
        none = 0,
        temporary = 1 << 0,
        /// Treat as if it has reference semantics.
        /// Used when aliasing references for inline functions.
        eval_as_reference = 1 << 1
      };

      struct value {
        /// TODO: Most of these are mutually exclusive. Could be a union.
        std::optional<std::string> identifier;
        /// Value stored in a register
        std::optional<vm::register_index> register_index;
        /// Constant value evaluated
        std::optional<vm::register_value> constant;
        /// Address stored in a register + address_offset
        std::optional<vm::register_index> indirect_register_index;
        /// Index of the symbol
        std::optional<size_t> symbol_index;
        /// Type of the value
        std::optional<size_t> type_index;
        /// Base address offset to the value (if applicable)
        int64_t address_offset = 0;

        value_flags flags = value_flags::none;
      };
      std::vector<value> value_stack;

      enum class instruction_tag : uint8_t {
        none,
        return_jmp, ///< Set jump instruction address to the start of the function return section
        stack_frame 
      };

      struct function {
        inline static constexpr int64_t CallLinkStorageSize = sizeof(vm::register_value) * 2;

        size_t symbol;
        size_t scope_id;
        size_t return_type;

        // size_t declaration_id; ///< ID of the declaration statement

        size_t args_size = 0;         ///< Size of the function parameters.
        size_t arg_count = 0;         ///< Number of arguments to this function.
        size_t temp_storage_used = 0; ///< Max temp storage allocated while evaluating this function
        size_t call_params_used  = 0; ///< Current size of the call parameters allocated

        size_t return_section_start = 0;

        std::vector<instruction_tag> instruction_tags;
        std::vector<vm::instruction> instructions;
      };
      std::vector<size_t>   function_stack;
      std::vector<function> functions;

      struct scope {
        std::vector<value> variables;
        std::vector<value> temporaries;
      };
      std::vector<scope> scopes;

      struct relocation {
        std::string_view symbol;
        uint64_t         offset;
        size_t           function_id;
      };

      /// Identifiers whose location needs to be resolved.
      /// [identifier] -> list of offsets into instructions. Offset is in bytes
      std::vector<relocation> relocations;

      void push_value(value r);
      std::optional<value> pop_value();

      bool begin_function(size_t symbol);
      void end_function();
      function & current_function();

      bool begin_scope();
      bool end_scope();

      void push_return_handler(const std::function<void(program_builder*)>& handler);
      void pop_return_handler();
      void return_with_return_handler();
      std::vector<std::function<void(program_builder*)>> return_handler_stack;

      /// Get a value that describes the return value.
      /// TODO: Might need to "push" return value for handling inline calls
      value get_return_value() const;
      void  push_return_value_receiver(value const & val);
      value pop_return_value_receiver();
      std::vector<value> return_values;

      /// Get the type of a value
      size_t get_value_type(value const & val) const;

      /// Add an identifier to the current scope
      void add_variable(program_builder::value const & val);

      std::optional<value> find_unnamed_initializer(size_t receiver, size_t initializer);

      /// Find a symbol by identifier. Searches from the inner most scope outwards.
      std::optional<value> find_value_by_identifier(std::string_view const & name) const;
      std::optional<value> find_value_by_identifier(std::string_view const & name, size_t scopeIndex) const;
      std::optional<value> find_value(std::function<bool(value const &)> const & predicate) const;
      std::optional<value> find_value(std::function<bool(value const &)> const & predicate, size_t scopeIndex) const;

      /// Allocate space for a temporary and push a value to the value_stack
      size_t allocate_temporary_value(size_t typeIndex);
      size_t allocate_temporary_call_parameter(size_t typeIndex);
      value get_temporary(size_t id) const;

      void free_temporary_value();

      void destroy_value(value * value);

      size_t current_scope_id() const;

      void add_relocation(std::string_view const& symbol, uint64_t offset);

      void call(value const & func);
      void call(uint64_t address);
      void call_indirect(vm::register_index const & symbol);
      void ret();

      void jump_to(value const & location);
      // void jump_to(program_metadata::symbol const & symbol);
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
      // vm::register_index load_value_of(program_metadata::symbol const & symbol);
      // vm::register_index load_value_of(uint64_t address, size_t size);
      vm::register_index load_value_of(program_builder::value const & value);
      // vm::register_index load_address_of(program_metadata::symbol const & symbol);
      // vm::register_index load_address_of(std::string_view identifier, size_t size);
      vm::register_index load_address_of(program_builder::value const & value);
      void release_register(vm::register_index reg);

      // expression_result pop_expression_result();
      void load(vm::register_index dst, vm::register_index address, size_t size, int64_t offset);
      void load(vm::register_index dst, vm::register_index address, size_t size);
      void load_from_constant_address(vm::register_index dst, vm::register_value address, size_t size);
      void move(vm::register_index dst, vm::register_index src);
      void set(vm::register_index dst, vm::register_value value);
      bool store(vm::register_index src, vm::register_index address, uint8_t sz);
      bool store(vm::register_index src, vm::register_index address, uint8_t sz, int64_t offset);
      void addi(vm::register_index dst, vm::register_index a, vm::register_index b);
      void addf(vm::register_index dst, vm::register_index a, vm::register_index b);
      void subi(vm::register_index dst, vm::register_index a, vm::register_index b);
      void subf(vm::register_index dst, vm::register_index a, vm::register_index b);
      void divi(vm::register_index dst, vm::register_index a, vm::register_index b);
      void divf(vm::register_index dst, vm::register_index a, vm::register_index b);
      void muli(vm::register_index dst, vm::register_index a, vm::register_index b);
      void mulf(vm::register_index dst, vm::register_index a, vm::register_index b);

      void add_instruction(vm::instruction inst);
      void set_instruction_tag(instruction_tag tag);

      /// Convert the program to a binary
      program binary() const;
    };
  }

  template<>
  struct enable_bitwise_ops<compiler::program_builder::value_flags> : std::true_type {};
}
