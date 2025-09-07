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
      jump_indirect,    ///< Set the program counter to a value stored in a register
      move,             ///< Move a value from a register
      compare_i64,      ///< Compare the values in two registers as integers
      compare_f64,      ///< Compare the values in two registers as floats
      conditional_jump, ///< Set the program counter if the specified comparison bits are set.
      conditional_move, ///< Compare the specified register with a value. Move if equal
      call,             ///< Call a function
      call_indirect,    ///< Call an address stored in a register
      ret,              ///< Return from a function
      // call_native,      ///< Call a native function
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
      uint64_t addr;
    };

    template<> struct op_code_args<op_code::jump_indirect> {
      register_index addr; // [reg]
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
        op_code_args<op_code::jump_indirect> jump_indirect;
        op_code_args<op_code::move> move;
        op_code_binary_op_args compare;
        op_code_args<op_code::conditional_jump> conditional_jump;
        op_code_args<op_code::conditional_move> conditional_move;
        op_code_args<op_code::call> call;
        op_code_args<op_code::call_indirect> call_indirect;
        op_code_args<op_code::ret> ret;
      };
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

    struct stack {
      stack(allocator* allocator)
      : allocator(allocator) {}

      /// Push data to the stack
      void * push(void const * data, size_t bytes) {
        void* ptr = allocate(bytes);
        memcpy(ptr, data, bytes);
        return ptr;
      }

      void * pop(void * dst, size_t bytes) {
        memcpy(dst, end() - bytes, bytes);
        return free(bytes);
      }

      /// Pop data from the stack
      void * free(size_t bytes) {
        size -= bytes;
        return base + size;
      }

      /// Allocate data on the stack.
      void * allocate(size_t bytes) {
        size_t newSize = size + bytes;
        if (!grow(newSize)) {
          return nullptr;
        }

        void* ptr = base + size;
        size = newSize;
        return ptr;
      }

      uint8_t* begin() { return base; }
      uint8_t* end() { return begin() + size; }
      uint8_t const * begin() const { return base; }
      uint8_t const * end() const { return begin() + size; }

      bool grow(size_t requiredCapacity) {
        if (requiredCapacity <= capacity)
          return true;

        size_t newCapacity = capacity;
        while (requiredCapacity > newCapacity)
          newCapacity = std::max(2ull, newCapacity * 2);

        return reserve(newCapacity);
      }

      bool reserve(size_t newCapacity) {
        if (newCapacity <= capacity) {
          return true;
        }

        uint8_t * newData = (uint8_t*)allocator->allocate(newCapacity);
        if (newData == nullptr) {
          return false;
        }

        memcpy(newData, base, size);
        allocator->free(base);
        base = newData;
        capacity = newCapacity;
        return true;
      }

      uint8_t * base = nullptr;
      uint64_t size = 0;
      uint64_t capacity = 0;
      allocator * allocator = nullptr;
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
        pc, ///< Program counter.
        fp, ///< Frame pointer. Offset of current stack frame.
        sp, ///< Stack pointer.
        rp, ///< Return pointer. Where the current scope should return to.
        count
      };
    };

    inline static constexpr size_t register_count = (size_t)register_names::count;

    struct machine {
      machine(allocator *allocator)
        : heap_allocator(allocator)
        , stack(allocator) {
        stack.reserve(4 * 1024 * 1024); // 4mb
        memset(registers, 0, sizeof(registers));
        memset(&next_instruction, 0, sizeof(next_instruction));
        registers[register_names::pc].u64 = 0;
        registers[register_names::fp].ptr = registers[register_names::sp].ptr = stack.base;
      }

      union {
        register_value value;
        uint64_t       u64;
        int64_t        i64;
        double         d64;
        void*          ptr;
      } registers[register_count];

      uint64_t program_counter() const {
        return registers[register_names::pc].u64;
      }

      instruction next_instruction;

      allocator * heap_allocator = nullptr;
      stack       stack;
    };

    bool decode(machine * vm);
    void relocate_program(program_view const & program);
    const_program_view load_program(machine * vm, program_view const& program, bool relocated = true);
    bool execute(machine * vm);
    bool step(machine *vm);
    void* compile_call_handle(machine* vm, program_symbol_table_entry const & symbol);

    void call(machine* vm, void * handle);
  }

  inline static std::string op_code_to_string(vm::op_code op) {
    switch (op) {
      case adder::vm::op_code::exit: return "exit";                         ///< Load a value from a memory address
      case adder::vm::op_code::load: return "load";                         ///< Load a value from a memory address
      case adder::vm::op_code::load_addr: return "load_addr";               ///< Load a value from a constant address
      case adder::vm::op_code::load_offset: return "load_offset";           ///< Load a value from an address (stored in a register) with some offset
      case adder::vm::op_code::store: return "store";                       ///< Store a value to a memory address
      case adder::vm::op_code::store_addr: return "store_addr";             ///< Store a value to a constant address
      case adder::vm::op_code::store_offset: return "store_offset";         ///< Store a value to an address (stored in a register) with some offset
      case adder::vm::op_code::set: return "set";                           ///< Set the value of a register
      case adder::vm::op_code::add_i64: return "add_i64";                   ///< Add two registers as integers
      case adder::vm::op_code::add_f64: return "add_f64";                   ///< Add two registers as floats
      case adder::vm::op_code::mul_i64: return "mul_i64";                   ///< Multiply two registers as integers
      case adder::vm::op_code::mul_f64: return "mul_f64";                   ///< Multiply two registers as floats
      case adder::vm::op_code::div_i64: return "div_i64";                   ///< Set the value of a register as integers
      case adder::vm::op_code::div_f64: return "div_f64";                   ///< Divide two registers as floats
      case adder::vm::op_code::alloc_stack: return "alloc_stack";           ///< Reserve space on the stack
      case adder::vm::op_code::free_stack: return "free_stack";             ///< Free space on the stack
      case adder::vm::op_code::push: return "push";                         ///< Push a register to the stack
      case adder::vm::op_code::pop: return "pop";                           ///< Pop a register value from the stack. Store in named register
      case adder::vm::op_code::jump: return "jump";                         ///< Set the program counter.
      case adder::vm::op_code::jump_indirect: return "jump_indirect";       ///< Set the program counter to a value stored in a register
      case adder::vm::op_code::move: return "move";                         ///< Move a value from a register
      case adder::vm::op_code::compare_i64: return "compare_i64";           ///< Compare the values in two registers as integers
      case adder::vm::op_code::compare_f64: return "compare_f64";           ///< Compare the values in two registers as floats
      case adder::vm::op_code::conditional_jump: return "conditional_jump"; ///< Set the program counter if the specified comparison bits are set.
      case adder::vm::op_code::conditional_move: return "conditional_move"; ///< Compare the specified register with a value. Move if equal
      case adder::vm::op_code::call: return "call"; ///< Compare the specified register with a value. Move if equal
      case adder::vm::op_code::call_indirect: return "call_indirect"; ///< Compare the specified register with a value. Move if equal
      case adder::vm::op_code::ret: return "ret"; ///< Compare the specified register with a value. Move if equal
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
