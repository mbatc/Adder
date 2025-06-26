#pragma once

#include <vector>
#include <list>

namespace adder {
  namespace vm {
    using register_value = uint64_t;
    using register_index = uint8_t;
    using address_t      = uint64_t;

    enum class op_code : uint64_t {
      exit,             ///< Load a value from a memory address
      load,             ///< Load a value from a memory address
      load_addr,        ///< Load a value from a constant address
      load_offset,      ///< Load a value from an address (stored in a register) with some offset
      store,            ///< Store a value to a memory address
      store_addr,       ///< Store a value to a constant address
      store_offset,     ///< Store a value to an address (stored in a register) with some offset
      set,              ///< Set the value of a register
      add_i64,          ///< Add two registers as integers
      add_f64,          ///< Add two registers as floats
      mul_i64,          ///< Multiply two registers as integers
      mul_f64,          ///< Multiply two registers as floats
      div_i64,          ///< Set the value of a register as integers
      div_f64,          ///< Divide two registers as floats
      alloc_stack,      ///< Reserve space on the stack
      free_stack,       ///< Free space on the stack
      push,             ///< Push a register to the stack
      pop,              ///< Pop a register value from the stack. Store in named register
      jump,             ///< Set the program counter.
      move,             ///< Move a value from a register
      compare_i64,      ///< Compare the values in two registers as integers
      compare_f64,      ///< Compare the values in two registers as floats
      conditional_jump, ///< Set the program counter if the specified comparison bits are set.
      conditional_move, ///< Compare the specified register with a value. Move if equal
      count,            ///< Number of op codes
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
      register_index addr;
      uint8_t        size;
      int64_t        offset;
    };

    template<> struct op_code_args<op_code::store> {
      register_index src;
      register_index addr; // [reg]
      uint8_t        size;
    };

    template<> struct op_code_args<op_code::store_addr> {
      register_index src;
      register_value addr;
      uint8_t        size;
    };

    template<> struct op_code_args<op_code::store_offset> {
      register_index src;
      register_index addr;
      uint8_t        size;
      int64_t        offset;
    };

    template<> struct op_code_args<op_code::set> {
      register_index dst;
      register_value val;
    };

    struct op_code_binary_op_args {
      register_index dst;
      register_index lhs;
      register_index rhs;
    };

    template<> struct op_code_args<op_code::add_i64> : op_code_binary_op_args {};
    template<> struct op_code_args<op_code::add_f64> : op_code_binary_op_args {};
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
      uint64_t dst_addr;
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

    inline static constexpr size_t op_code_count = (size_t)op_code::count;

    size_t instruction_size(op_code code);

    /// Variable number of arguments.
    /// Dependent on op code implementation
    struct instruction {
      op_code  code;
      union {
        uint8_t arg_bytes[1];
        op_code_args<op_code::load> load;
        op_code_args<op_code::load_offset> load_offset;
        op_code_args<op_code::load_addr> load_addr;
        op_code_args<op_code::store> store;
        op_code_args<op_code::store_offset> store_offset;
        op_code_args<op_code::store_addr> store_addr;
        op_code_args<op_code::set> set;
        op_code_binary_op_args add;
        op_code_binary_op_args mul;
        op_code_binary_op_args div;
        op_code_args<op_code::alloc_stack> alloc_stack;
        op_code_args<op_code::free_stack> free_stack;
        op_code_args<op_code::push> push;
        op_code_args<op_code::pop> pop;
        op_code_args<op_code::jump> jump;
        op_code_args<op_code::move> move;
        op_code_binary_op_args compare;
        op_code_args<op_code::conditional_jump> conditional_jump;
        op_code_args<op_code::conditional_move> conditional_move;
      };
    };

    struct allocator {
      struct block {
        uint64_t offset;
        uint64_t size;
      };

      std::list<block>     blocks; ///< Available blocks
      std::vector<uint8_t> data;   ///< Allocated data for this heap.

      uint8_t const * read(uint64_t stack_address) const;
      uint8_t * read(uint64_t stack_address);
      size_t write(uint64_t stack_address, uint8_t const * bytes, size_t count);
      uint64_t allocate(size_t size);
      void free(uint64_t block);
    };

    struct memory {
      inline static constexpr uint64_t stack_top = std::numeric_limits<uint64_t>::max();

      /// Allocate heap memory.
      uint64_t allocate(size_t size);

      /// Free a heap allocation.
      void free(uint64_t stack_address);

      uint64_t allocate_stack(size_t size);

      /// Push data to the stack
      uint64_t push(uint8_t const * data, size_t size);

      /// Pop data from the stack
      uint64_t pop(size_t bytes);

      uint64_t  stack_bottom() const;
      bool      is_stack(uint64_t stack_address) const;

      uint8_t * read(uint64_t stack_address);
      uint8_t const * read(uint64_t stack_address) const;

      size_t write(uint64_t stack_address, uint8_t const * bytes, size_t count);
      
      uint64_t to_stack_offset(uint64_t address);
      uint8_t * read_stack(uint64_t offset);
      uint8_t const * read_stack(uint64_t offset) const;
      size_t write_stack(uint64_t offset, uint8_t const * bytes, size_t count);

      std::vector<uint8_t> stack; ///< Stack allocator
      uint64_t             stack_size = 0;
      allocator            heap;  ///< Heap allocator
    };

    enum class register_names {
      r0, ///< General IO 0
      r1, ///< General IO 1
      r2, ///< General IO 2
      r3, ///< General IO 3
      r4, ///< General IO 4
      r5, ///< General IO 5
      r6, ///< General IO 6
      fp, ///< Frame pointer. Offset of current stack frame.
      sp, ///< Stack pointer. 
      count
    };
    inline static constexpr size_t register_count = (size_t)register_names::count;

    struct machine {
      machine() {
        memset(registers, 0, sizeof(registers));
        memset(&next_instruction, 0, sizeof(next_instruction));
        program_counter = 0;
        registers[(uint8_t)register_names::fp] = registers[(uint8_t)register_names::sp] = memory.stack_top;
      }

      register_value registers[register_count];
      uint64_t       program_counter;  ///< Address of next instruction to execute
      instruction    next_instruction;
      memory         memory;
    };

    bool decode(machine * vm);
    void relocate_program(uint8_t * program, size_t size, uint64_t base);
    uint64_t load_program(machine * vm, std::vector<uint8_t> const & program);
    bool execute(machine * vm);

    bool step(machine *vm);
  }
}
