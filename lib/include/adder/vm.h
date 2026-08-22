#pragma once

#include <vector>
#include <list>
#include <string>

namespace adder {
  template<bool Const>
  struct program_view_impl;
  struct program;
  struct program_symbol_table_entry;
  using program_view       = program_view_impl<false>;
  using const_program_view = program_view_impl<true>;

  namespace vm {
    using register_value = uint64_t;
    using register_index = uint8_t;
    using address_t      = uint64_t;

    struct call_context;
    struct machine;

    uint8_t * call_context_read_arg(call_context * ctx, size_t sz);
    machine * call_context_get_machine(call_context * ctx);
    void    * call_context_get_user_data(call_context * ctx);
    
    using native_method_t = void (*)(call_context *);

    struct native_method_binding {
      native_method_t callback;
      void *          user_data;
    };

    enum class op_code : uint8_t {
      exit,               ///< Load a value from a memory address
      noop,               ///< Do nothing
      load,               ///< Load a value from a memory address
      load_addr,          ///< Load a value from a constant address
      load_offset,        ///< Load a value from an address (stored in a register) with some offset
      store,              ///< Store a value to a memory address
      store_addr,         ///< Store a value to a constant address
      store_offset,       ///< Store a value to an address (stored in a register) with some offset
      store_value,        ///< Store a constant value to a memory address
      store_value_addr,   ///< Store a constant value to a constant address
      store_value_offset, ///< Store a constant value to an address (stored in a register) with some offset
      set,                ///< Set the value of a register
      itof32,             ///< Convert the value in src from an integer to a float. Store the result in dst.
      itof64,             ///< Convert the value in src from an integer to a float. Store the result in dst.
      f32toi,             ///< Convert the value in src from a float to an integer. Store the result in dst.
      f64toi,             ///< Convert the value in src from a float to an integer. Store the result in dst.
      add_i64,            ///< Add two registers as integers
      add_i64_constant,   ///< Add two integers lhs is a register, rhs is a constant
      add_f64,            ///< Add two registers as floats
      sub_i64,            ///< Add two registers as integers
      sub_f64,            ///< Add two registers as floats
      mul_i64,            ///< Multiply two registers as integers
      mul_f64,            ///< Multiply two registers as floats
      div_i64,            ///< Set the value of a register as integers
      div_f64,            ///< Divide two registers as floats
      alloc_stack,        ///< Reserve space on the stack
      free_stack,         ///< Free space on the stack
      push,               ///< Push a register to the stack
      pop,                ///< Pop a register value from the stack. Store in named register
      jump,               ///< Set the program counter.
      jump_relative,      ///< Add a value to the program counter
      jump_if_zero_relative, ///< Add a value to the program counter if the comparison register is zero
      jump_indirect,      ///< Set the program counter to a value stored in a register
      move,               ///< Move a value from a register

      bitwise_and,        ///< Perform a bitwise-and on two registers. Store the result in lhs
      bitwise_or,         ///< Perform a bitwise-or on two registers. Store the result in lhs
      bitwise_xor,        ///< Perform a bitwise-xor on two registers. Store the result in lhs
      bitwise_and_value,  ///< Perform a bitwise-and on two registers. Store the result in lhs
      bitwise_or_value,   ///< Perform a bitwise-or on two registers. Store the result in lhs
      bitwise_xor_value,  ///< Perform a bitwise-xor on two registers. Store the result in lhs

      set_non_zero,       ///< Load a value from a memory address

      compare_i64,        ///< Compare the values in two registers as integers
      compare_f64,        ///< Compare the values in two registers as floats
      conditional_jump_relative, ///< Add a value to the program counter if the specified comparison bits are set.
      conditional_move,   ///< Compare the specified register with a value. Move if equal
      call,               ///< Call a function
      call_indirect,      ///< Call an address stored in a register
      call_native,        ///< Call a native function. This instruction calls VMNativeMethod
      ret,                ///< Return from a function
      // call_native,     ///< Call a native function
      count,              ///< Number of op codes
    };

    template<op_code code>
    struct op_code_args {};

    template<> struct op_code_args<op_code::exit> {};

    template<> struct op_code_args<op_code::load> {
      register_index dst;
      register_index src_addr; // [reg]
      uint8_t        size;
    };

    template<> struct op_code_args<op_code::load_addr> {
      register_index dst;
      register_value addr; // Constant address within the program. Must be relocated when the program is loaded.
      uint8_t        size;
    };

    template<> struct op_code_args<op_code::load_offset> {
      register_index dst;
      register_index src_addr;
      uint8_t        size;
      int64_t        offset;
    };

    template<> struct op_code_args<op_code::store> {
      register_index addr; // [reg]
      register_index src;
      uint8_t        size;
    };

    template<> struct op_code_args<op_code::store_addr> {
      register_value addr;
      register_index src;
      uint8_t        size;
    };

    template<> struct op_code_args<op_code::store_offset> {
      register_index addr;
      register_index src;
      uint8_t        size;
      int64_t        offset;
    };

    template<> struct op_code_args<op_code::store_value> {
      register_value src;
      register_index addr; // [reg]
      uint8_t        size;
    };

    template<> struct op_code_args<op_code::store_value_addr> {
      register_value addr;
      register_value src;
      uint8_t        size;
    };

    template<> struct op_code_args<op_code::store_value_offset> {
      register_value src;
      register_index addr;
      uint8_t        size;
      int64_t        offset;
    };

    template<> struct op_code_args<op_code::set> {
      register_value val;
      register_index dst;
    };

    struct op_code_xtox_args {
      register_index dst;
      register_index src;
    };

    struct op_code_binary_op_args {
      register_index dst;
      register_index lhs;
      register_index rhs;
    };

    struct op_code_binary_op_args_reg_constant {
      register_value rhs;
      register_index dst;
      register_index lhs;
    };
    
    struct op_code_bitwise_op_args {
      register_index lhs;
      register_index rhs;
    };

    struct op_code_bitwise_op_constant_args {
      register_value val;
      register_index reg;
    };
    
    template<> struct op_code_args<op_code::set_non_zero> {
      register_index dst;
      uint8_t if_non_zero;
      uint8_t if_zero;
    };

    template<> struct op_code_args<op_code::add_i64> : op_code_binary_op_args {};
    template<> struct op_code_args<op_code::add_i64_constant> : op_code_binary_op_args_reg_constant {};
    template<> struct op_code_args<op_code::add_f64> : op_code_binary_op_args {};
    template<> struct op_code_args<op_code::sub_i64> : op_code_binary_op_args {};
    template<> struct op_code_args<op_code::sub_f64> : op_code_binary_op_args {};
    template<> struct op_code_args<op_code::mul_i64> : op_code_binary_op_args {};
    template<> struct op_code_args<op_code::mul_f64> : op_code_binary_op_args {};
    template<> struct op_code_args<op_code::div_i64> : op_code_binary_op_args {};
    template<> struct op_code_args<op_code::div_f64> : op_code_binary_op_args {};

    template<> struct op_code_args<op_code::alloc_stack> {
      uint32_t bytes;
    };

    template<> struct op_code_args<op_code::free_stack> {
      uint32_t bytes;
    };

    template<> struct op_code_args<op_code::push> {
      register_index src;
      uint8_t        size;
    };

    template<> struct op_code_args<op_code::pop> {
      register_index dst;
      uint8_t        size;
    };

    template<> struct op_code_args<op_code::jump> {
      uint64_t addr;
    };

    template<> struct op_code_args<op_code::jump_indirect> {
      register_index addr; // [reg]
    };

    template<> struct op_code_args<op_code::jump_relative> {
      int64_t offset;
    };

    template<> struct op_code_args<op_code::jump_if_zero_relative> {
      int64_t        offset;
      register_index cmp;
    };

    template<> struct op_code_args<op_code::move> {
      register_index dst;
      register_index src;
    };

    inline static constexpr uint8_t cmp_lt_bit = 1 << 0;
    inline static constexpr uint8_t cmp_eq_bit = 1 << 1;
    inline static constexpr uint8_t cmp_gt_bit = 1 << 2;

    template<> struct op_code_args<op_code::compare_i64> : op_code_binary_op_args {};
    template<> struct op_code_args<op_code::compare_f64> : op_code_binary_op_args {};

    template<> struct op_code_args<op_code::conditional_jump_relative> : op_code_args<op_code::jump_relative> {
      register_index cmp_reg;
      uint8_t        cmp_val;
    };

    template<> struct op_code_args<op_code::conditional_move> : op_code_args<op_code::move> {
      register_index cmp_reg;
      uint8_t        cmp_val;
    };

    template<> struct op_code_args<op_code::call> {
      register_value addr;
    };

    template<> struct op_code_args<op_code::call_indirect> {
      register_index addr;
    };

    template<> struct op_code_args<op_code::call_native> {
      address_t native_method_index; ///< Callback bound via the extern_method_lookup.
    };

    template<> struct op_code_args<op_code::ret> {};

    inline static constexpr size_t op_code_count = (size_t)op_code::count;

    size_t instruction_size(op_code code);

    /// Variable number of arguments.
    /// Dependent on op code implementation
    struct instruction {
      union {
        uint8_t arg_bytes[1];
        op_code_args<op_code::load> load;
        op_code_args<op_code::load_offset> load_offset;
        op_code_args<op_code::load_addr> load_addr;
        op_code_args<op_code::store> store;
        op_code_args<op_code::store_offset> store_offset;
        op_code_args<op_code::store_addr> store_addr;
        op_code_args<op_code::store_value> store_value;
        op_code_args<op_code::store_value_offset> store_value_offset;
        op_code_args<op_code::store_value_addr> store_value_addr;
        op_code_args<op_code::set> set;
        op_code_xtox_args xtox;
        op_code_binary_op_args add;
        op_code_args<op_code::add_i64_constant> add_constant;
        op_code_binary_op_args sub;
        op_code_binary_op_args mul;
        op_code_binary_op_args div;
        op_code_args<op_code::alloc_stack> alloc_stack;
        op_code_args<op_code::free_stack> free_stack;
        op_code_args<op_code::push> push;
        op_code_args<op_code::pop> pop;
        op_code_args<op_code::jump> jump;
        op_code_args<op_code::jump_indirect> jump_indirect;
        op_code_args<op_code::jump_relative> jump_relative;
        op_code_args<op_code::jump_if_zero_relative> jump_if_zero_relative;
        op_code_args<op_code::move> move;
        op_code_bitwise_op_args bitwise_op;
        op_code_bitwise_op_constant_args bitwise_op_constant;
        op_code_args<op_code::set_non_zero> set_non_zero;
        op_code_binary_op_args compare;
        op_code_args<op_code::conditional_jump_relative> conditional_jump_relative;
        op_code_args<op_code::conditional_move> conditional_move;
        op_code_args<op_code::call> call;
        op_code_args<op_code::call_indirect> call_indirect;
        op_code_args<op_code::call_native> call_native;
        op_code_args<op_code::ret> ret;
      };
      op_code code;
    };

    struct allocator {
      // struct block {
      //   uint64_t offset;
      //   uint64_t size;
      // };
      // std::list<block>     blocks; ///< Available blocks
      // std::vector<uint8_t> data;   ///< Allocated data for this heap.

      void * allocate(size_t size);
      void   free(void * ptr);
    };

    struct register_names {
      enum {
        r0, ///< General IO 0
        r1, ///< General IO 1
        r2, ///< General IO 2
        r3, ///< General IO 3
        r4, ///< General IO 4
        r5, ///< General IO 5
        r6, ///< General IO 6
        gp_end,
        pc = gp_end, ///< Program counter.
        fp, ///< Frame pointer. Offset of current stack frame.
        sp, ///< Stack pointer.
        rp, ///< Return pointer. Where the current scope should return to.
        count
      };
    };

    inline static constexpr size_t register_count = (size_t)register_names::count;

    struct machine {
      machine(allocator * allocator)
        : heap_allocator(allocator) {
        const size_t initialStackSize = 4 * 1024 * 1024; // 4mb
        stack.base = (uint8_t*)heap_allocator->allocate(initialStackSize);
        stack.end  = stack.base + initialStackSize;
        memset(registers, 0, sizeof(registers));

        registers[register_names::pc].u64 = 0;
        registers[register_names::fp].ptr = registers[register_names::sp].ptr = stack.base;
      }

      union {
        register_value value;
        uint64_t       u64;
        int64_t        i64;
        double         f64;
        float          f32;
        void*          ptr;
        uint8_t*       data;
      } registers[register_count];

      uint64_t program_counter() const {
        return registers[register_names::pc].u64;
      }

      struct {
        uint8_t * base = nullptr;
        uint8_t * end  = nullptr;
      } stack;

      allocator * heap_allocator = nullptr;

      void * user_data = nullptr;

      native_method_binding (*lookup_extern_symbol)(machine * vm, char const * symbol) = nullptr;

      std::string (*load_module_source)(machine * vm, char const * module_name);

      address_t load_extern_symbol(char const * symbol) {
        auto it = std::find_if(
          registered_extern_method_names.begin(),
          registered_extern_method_names.end(),
          [symbol](const std::string & o) { return o == symbol; }
        );
        if (it != registered_extern_method_names.end())
          return it - registered_extern_method_names.begin();

        native_method_binding binding = lookup_extern_symbol(this, symbol);
        registered_extern_method_names.push_back(symbol);
        registered_extern_methods.push_back(binding);
        return registered_extern_methods.size() - 1;
      }

      std::vector<std::string>           registered_extern_method_names;
      std::vector<native_method_binding> registered_extern_methods;
    };

    void relocate_program(machine * vm, program_view const & program);

    const_program_view load_program(machine * vm, program_view const & program, bool relocated = true);

    void* compile_call_handle(machine* vm, program_symbol_table_entry const & symbol);
    void* compile_call_handle(machine* vm, address_t const & routineAddress);
    void free(machine* vm, void * ptr);

    /// Push a parameter for `call`.
    /// You must ensure the correct parameters are allocated.
    /// The first parameter is the return value destination.
    /// The is function simply allocates storage for the parameters.
    /// It is the callers responsibility to construct the variables.
    void* call_push_parameter(machine* vm, size_t bytes);
    /// Pop a call parameter.
    /// It is up to the caller to pop the correct number of bytes and to destruct the parameters.
    void* call_pop_parameter(machine* vm, size_t bytes);
    void call(machine* vm, void * handle);
  }

  std::string op_code_to_string(vm::op_code op);
  std::string register_to_string(size_t idx);
}

#define AD_IOFFSET(member) ((size_t)&((adder::vm::instruction*)0)->member)
