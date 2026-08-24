#pragma once

#include "types.h"
#include "program_metadata.h"
#include "ast.h"
#include "ast/builtins.h"
#include "../containers/pool.h"

#include <functional>

namespace adder {
  struct program;
  enum class relocation_linkage : uint8_t;

  namespace compiler {
    struct program_builder {
      program_builder() {}

      std::shared_ptr<program_metadata> meta;

      enum class value_flags {
        none = 0,
        /// Value uses temporary storage space
        temporary = 1 << 0,
        /// Value uses stack-frame storage space
        stack_variable = 1 << 1,
        /// Treat as if it has reference semantics.
        /// Used when aliasing references for inline functions.
        eval_as_reference = 1 << 2,
        /// Treat as if it has reference semantics.
        /// Used when aliasing references for inline functions.
        alias = 1 << 3,
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
        std::optional<symbol_reference> symbol_index;
        /// Type of the value
        std::optional<type_reference> type_info;
        /// Base address offset to the value (if applicable)
        int64_t address_offset = 0;

        value_flags flags = value_flags::none;
      };

      struct Registers {
        struct RegisterState {
          // std::optional<value> value;
          // std::optional<value> address;

          int64_t              numPins = 0;
          size_t               lastUsed = 0;
        };

        RegisterState states[vm::register_names::gp_end];
        vm::register_index next = 0;
        size_t useRound = 0;

        /// Pin a register new register.
        vm::register_index pin();
        vm::register_index pin(vm::register_index idx);
        /// Find and pin a register that already has `value` loaded.
        // std::optional<vm::register_index> find_and_pin(const value &value);
        /// Release a pinned register.
        void release(vm::register_index idx);
        /// Evict all known values from register states.
        // void evict();

        /// void realise_value();
      } registers;

      std::vector<value> value_stack;

      enum class instruction_tag : uint8_t {
        none,
        return_jmp, ///< Set jump instruction address to the start of the function return section
        stack_frame,
        add_temporary_storage_offset,
        add_stack_storage_offset,
      };

      struct function {
        inline static constexpr int64_t CallLinkStorageSize = sizeof(vm::register_value) * 2;

        symbol_reference      symbol;
        std::optional<size_t> scope_id; ///< Extern functions don't have a scope id
        type_reference        return_type = type_reference::undefined();

        size_t args_size = 0; ///< Size of the function parameters.
        // size_t arg_count = 0;         ///< Number of arguments to this function.
        // size_t call_params_used   = 0; ///< Current size of the call parameters allocated

        size_t stack_storage_used = 0; ///< Current size of the allocated stack storage
        size_t max_stack_storage  = 0; ///< Max stack storage allocated while evaluating this function
        size_t temp_storage_used  = 0; ///< Current size of the allocated temporary storage
        size_t max_temp_storage   = 0; ///< Max temp storage allocated while evaluating this function

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

      struct scoped_temporary_cleaner {
        scoped_temporary_cleaner(program_builder* builder, size_t scopeId)
          : builder(builder)
          , scope_id(scopeId)
          , initial_temporary_count(builder->scopes[scopeId].temporaries.size())
        {}

        ~scoped_temporary_cleaner() {
          while (builder->scopes[scope_id].temporaries.size() > initial_temporary_count)
            builder->free_temporary_value();
        }

        program_builder * builder;
        const size_t scope_id;
        const size_t initial_temporary_count;
      };

      struct relocation {
        relocation_linkage linkage;
        std::string_view   module_name;
        std::string_view   symbol;
        uint64_t           offset;
        size_t             function_id;
      };

      /// Identifiers whose location needs to be resolved.
      /// [identifier] -> list of offsets into instructions. Offset is in bytes
      std::vector<relocation> relocations;

      void push_value(value r);
      std::optional<value> pop_value();

      bool begin_function(symbol_reference symbol);
      void end_function();
      void end_function(function *func);
      function & current_function();

      bool begin_scope();
      bool end_scope();
      void emit_scope_cleanup();
      void emit_scope_cleanup(size_t upToScopeId);

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
      type_reference get_value_type(value const & val) const;

      /// Allocate stack space for a variable.
      /// Returns the frame-pointer offset to the new variable.
      program_builder::value allocate_stack_variable(type_reference typeIndex);

      /// Add an identifier to the current scope
      void add_variable(program_builder::value const & val);

      std::optional<value> find_unnamed_initializer(type_reference receiver, type_reference initializer);
      std::optional<value> find_operator(expr::operator_type op, type_reference lhs, type_reference rhs);

      /// Find a symbol by identifier. Searches from the inner most scope outwards.
      std::optional<value> find_value_by_identifier(std::string_view const & name) const;
      std::optional<value> find_value_by_identifier(std::string_view const & name, size_t scopeIndex) const;
      std::optional<value> find_value(std::function<bool(value const &)> const & predicate) const;
      std::optional<value> find_value(std::function<bool(value const &)> const & predicate, size_t scopeIndex) const;

      /// Allocate space for a temporary and push a value to the value_stack
      value allocate_temporary_value(type_reference typeInfo);

      size_t allocate_temporary_call_parameter(type_reference typeIndex);
      value get_temporary(size_t id) const;

      void free_temporary_value();

      void destroy_value(value * value);

      size_t current_scope_id() const;

      void add_relocation(program_metadata const * meta, std::string_view const & symbol, uint64_t offset);
      void add_relocation(relocation_linkage const & linkage, std::string_view const & module_name,
                          std::string_view const & symbol, uint64_t offset);

      void call(value const & func);
      void call(uint64_t address);
      void call_indirect(vm::register_index const & symbol);
      void call_native(symbol_reference const & symbol);
      void ret();

      void jump_to(value const & location);
      // void jump_to(program_metadata::symbol const & symbol);
      void jump_to(uint64_t address);
      void jump_indirect(vm::register_index const & address);
      void jump_relative(int64_t offset);
      void jump_if_zero_rel(int64_t offset, vm::register_index dst);
      
      void comparei(vm::register_index dst, vm::register_index a, vm::register_index b);
      void comparef(vm::register_index dst, vm::register_index a, vm::register_index b);

      void push_return_pointer();
      void push_frame_pointer();
      void pop_return_pointer();
      void pop_frame_pointer();

      void alloc_stack(size_t bytes);
      void free_stack(size_t bytes);

      /// Push a register value to the stack
      void push(vm::register_index const & src);
      /// Pop a register value from the stack
      void pop(vm::register_index const & dst);

      vm::register_index pin_register();
      vm::register_index load_constant(vm::register_value value);
      vm::register_index load_value_of(program_builder::value const & value);
      vm::register_index load_address_of(program_builder::value const & value);
      void release_register(vm::register_index reg);

      void load(vm::register_index dst, vm::register_index address, size_t size, int64_t offset);
      void load(vm::register_index dst, vm::register_index address, size_t size);
      void load_from_constant_address(vm::register_index dst, vm::register_value address, size_t size);
      void move(vm::register_index dst, vm::register_index src);
      void set(vm::register_index dst, vm::register_value value);
      void itof(vm::register_index dst, vm::register_index src, uint8_t fltSize);
      void ftoi(vm::register_index dst, vm::register_index src, uint8_t fltSize);

      bool store(vm::register_index src, vm::register_index address, uint8_t sz);
      bool store(vm::register_index src, vm::register_index address, uint8_t sz, int64_t offset);
      bool store_constant(vm::register_value src, vm::register_index dst, uint8_t sz);
      bool store_constant(vm::register_value src, vm::register_index dst, uint8_t sz, int64_t offset);
      bool store_constant(vm::register_value src, program_builder::value const & dst);
      bool store_to_constant_address(vm::register_index src, vm::register_value dst, uint8_t sz);
      bool store_constant_to_constant_address(vm::register_value src, vm::register_value dst, uint8_t sz);

      bool store(vm::register_index src, program_builder::value const & dst);
      // bool store(program_builder::value const & src, program_builder::value const & dst);

      void bitwise_and(vm::register_index dst, vm::register_index val);
      void bitwise_or(vm::register_index dst, vm::register_index val);
      void bitwise_xor(vm::register_index dst, vm::register_index val);
      
      void bitwise_and_constant(vm::register_index dst, vm::register_value val);
      void bitwise_or_constant(vm::register_index dst, vm::register_value val);
      void bitwise_xor_constant(vm::register_index dst, vm::register_value val);

      void set_non_zero(vm::register_index reg, uint8_t ifNonZero, uint8_t ifZero);

      void addi(vm::register_index dst, vm::register_index a, vm::register_index b);
      void addi_constant(vm::register_index dst, vm::register_index a, vm::register_value b);
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
