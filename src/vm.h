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
      conditional_jump,   ///< Set the program counter if the specified comparison bits are set.
      conditional_move,   ///< Compare the specified register with a value. Move if equal
      call,               ///< Call a function
      call_indirect,      ///< Call an address stored in a register
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

    template<> struct op_code_args<op_code::move> {
      register_index dst;
      register_index src;
    };

    inline static constexpr uint8_t cmp_lt_bit = 1 << 0;
    inline static constexpr uint8_t cmp_eq_bit = 1 << 1;
    inline static constexpr uint8_t cmp_gt_bit = 1 << 2;

    template<> struct op_code_args<op_code::compare_i64> : op_code_binary_op_args {};
    template<> struct op_code_args<op_code::compare_f64> : op_code_binary_op_args {};

    template<> struct op_code_args<op_code::conditional_jump> : op_code_args<op_code::jump> {
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
        op_code_args<op_code::move> move;
        op_code_bitwise_op_args bitwise_op;
        op_code_bitwise_op_constant_args bitwise_op_constant;
        op_code_args<op_code::set_non_zero> set_non_zero;
        op_code_binary_op_args compare;
        op_code_args<op_code::conditional_jump> conditional_jump;
        op_code_args<op_code::conditional_move> conditional_move;
        op_code_args<op_code::call> call;
        op_code_args<op_code::call_indirect> call_indirect;
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
      machine(allocator *allocator)
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
        double         d64;
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
    };

    void relocate_program(program_view const & program);
    const_program_view load_program(machine * vm, program_view const& program, bool relocated = true);

    /// Call handle should include metadata for the symbol return/parameter types.
    /// `call` should take a callback used to initialize parameters and receive the return value
    ///   e.g. typedef (*CallParameterInitializer)(void * ptr, int position, type * type, void *userdata);
    ///        typedef (*ReturnValueHandler)(void const * ptr, type * type, void *userdata);
    void* compile_call_handle(machine* vm, program_symbol_table_entry const & symbol);
    void free(machine* vm, void * ptr);
    void call(machine* vm, void * handle);
  }

  inline static std::string op_code_to_string(vm::op_code op) {
    switch (op) {
      case adder::vm::op_code::exit: return "exit";                             ///< Stop program execution
      case adder::vm::op_code::noop: return "no-op";                            ///< No op
      case adder::vm::op_code::load: return "load";                             ///< Load a value from a memory address
      case adder::vm::op_code::load_addr: return "load_addr";                   ///< Load a value from a constant address
      case adder::vm::op_code::load_offset: return "load_offset";               ///< Load a value from an address (stored in a register) with some offset
      case adder::vm::op_code::store: return "store";                           ///< Store a value to a memory address
      case adder::vm::op_code::store_addr: return "store_addr";                 ///< Store a value to a constant address
      case adder::vm::op_code::store_offset: return "store_offset";             ///< Store a value to an address (stored in a register) with some offset
      case adder::vm::op_code::store_value: return "store_value";               ///< Store a constant to a memory address
      case adder::vm::op_code::store_value_addr: return "store_value_addr";     ///< Store a constant to a constant address
      case adder::vm::op_code::store_value_offset: return "store_value_offset"; ///< Store a constant to an address (stored in a register) with some offset
      case adder::vm::op_code::set: return "set";                               ///< Set the value of a register
      case adder::vm::op_code::add_i64: return "add_i64";                       ///< Add two registers as integers
      case adder::vm::op_code::add_i64_constant: return "add_i64_constant";     ///< Add two registers as integers
      case adder::vm::op_code::add_f64: return "add_f64";                       ///< Add two registers as floats
      case adder::vm::op_code::sub_i64: return "add_i64";                       ///< Subtract two registers as integers
      case adder::vm::op_code::sub_f64: return "add_f64";                       ///< Subtract two registers as floats
      case adder::vm::op_code::mul_i64: return "mul_i64";                       ///< Multiply two registers as integers
      case adder::vm::op_code::mul_f64: return "mul_f64";                       ///< Multiply two registers as floats
      case adder::vm::op_code::div_i64: return "div_i64";                       ///< Set the value of a register as integers
      case adder::vm::op_code::div_f64: return "div_f64";                       ///< Divide two registers as floats
      case adder::vm::op_code::alloc_stack: return "alloc_stack";               ///< Reserve space on the stack
      case adder::vm::op_code::free_stack: return "free_stack";                 ///< Free space on the stack
      case adder::vm::op_code::push: return "push";                             ///< Push a register to the stack
      case adder::vm::op_code::pop: return "pop";                               ///< Pop a register value from the stack. Store in named register
      case adder::vm::op_code::jump: return "jump";                             ///< Set the program counter.
      case adder::vm::op_code::jump_indirect: return "jump_indirect";           ///< Set the program counter to a value stored in a register
      case adder::vm::op_code::jump_relative: return "jump_relative";           ///< Set the program counter to a location relative to the current instruction
      case adder::vm::op_code::move: return "move";                             ///< Move a value from a register
      case adder::vm::op_code::bitwise_and: return "bitwise_and";               ///< Move a value from a register
      case adder::vm::op_code::bitwise_or: return "bitwise_or";                 ///< Move a value from a register
      case adder::vm::op_code::bitwise_xor: return "bitwise_xor";               ///< Move a value from a register
      case adder::vm::op_code::bitwise_and_value: return "bitwise_and_value";   ///< Move a value from a register
      case adder::vm::op_code::bitwise_or_value: return "bitwise_or_value";     ///< Move a value from a register
      case adder::vm::op_code::bitwise_xor_value: return "bitwise_xor_value";   ///< Move a value from a register
      case adder::vm::op_code::set_non_zero: return "set_non_zero";             ///< Move a value from a register
      case adder::vm::op_code::compare_i64: return "compare_i64";               ///< Compare the values in two registers as integers
      case adder::vm::op_code::compare_f64: return "compare_f64";               ///< Compare the values in two registers as floats
      case adder::vm::op_code::conditional_jump: return "conditional_jump";     ///< Set the program counter if the specified comparison bits are set.
      case adder::vm::op_code::conditional_move: return "conditional_move";     ///< Compare the specified register with a value. Move if equal
      case adder::vm::op_code::call: return "call";                             ///< Compare the specified register with a value. Move if equal
      case adder::vm::op_code::call_indirect: return "call_indirect";           ///< Compare the specified register with a value. Move if equal
      case adder::vm::op_code::ret: return "ret";                               ///< Compare the specified register with a value. Move if equal
      default: return "unknown";
    }
  }
  
  inline static std::string register_to_string(size_t idx) {
    const std::string names[adder::vm::register_names::count] = {
      "r0",
      "r1",
      "r2",
      "r3",
      "r4",
      "r5",
      "r6",
      "pc",
      "fp",
      "sp",
      "rp"
    };
    return idx >= adder::vm::register_names::count ? "unknown" : names[idx];
  }
}

#define AD_IOFFSET(member) ((size_t)&((adder::vm::instruction*)0)->member)
