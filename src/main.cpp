
//#include <iostream>
//#include <vector>
//#include <list>
//#include <map>
//#include <algorithm>
//#include <optional>
//#include <functional>
//#include <variant>
//#include <string>
//#include <fstream>
//
//namespace adder {
//  template<typename... Args>
//  void unused(Args const & ...) {}
//
//  template<typename T>
//  struct enable_bitwise_ops : std::false_type {};
//
//  template<typename T>
//  inline static constexpr bool enable_bitwise_ops_v = enable_bitwise_ops<T>::value;
//}
//template<typename T, std::enable_if_t<adder::enable_bitwise_ops_v<T>>* = 0>
//T operator|(T const & a, T const & b) {
//  return T(std::underlying_type_t<T>(a) | std::underlying_type_t<T>(b));
//}
//
//template<typename T, std::enable_if_t<adder::enable_bitwise_ops_v<T>>* = 0>
//T operator&(T const & a, T const & b) {
//  return T(std::underlying_type_t<T>(a) & std::underlying_type_t<T>(b));
//}
//
//template<typename T, std::enable_if_t<adder::enable_bitwise_ops_v<T>>* = 0>
//T operator^(T const & a, T const & b) {
//  return T(std::underlying_type_t<T>(a) & std::underlying_type_t<T>(b));
//}
//
//template<typename T, std::enable_if_t<adder::enable_bitwise_ops_v<T>>* = 0>
//T & operator|=(T & a, T const & b) {
//  return a = a | b;
//}
//
//template<typename T, std::enable_if_t<adder::enable_bitwise_ops_v<T>>* = 0>
//T & operator&=(T & a, T const & b) {
//  return a = a & b;
//}
//
//template<typename T, std::enable_if_t<adder::enable_bitwise_ops_v<T>>* = 0>
//T & operator^=(T & a, T const & b) {
//  return a = a ^ b;
//}
//
//namespace adder {
//  namespace str {
//    inline static const std::vector<std::string> whitespace = { " ", "\n", "\r", "\t", "\v" };
//
//    std::string_view trim_start(std::string_view const & str, std::string_view const & chars = " \n\r\t\v") {
//      auto it = str.begin();
//      for (; it < str.end(); ++it)
//        if (chars.find(*it) == std::string::npos)
//          break;
//      return str.substr(it - str.begin());
//    }
//
//    std::string_view trim_end(std::string_view const & str, std::string_view const & chars = " \n\r\t\v") {
//      if (str.length() == 0)
//        return {};
//      auto it = str.end() - 1;
//      for (; it >= str.begin(); --it)
//        if (chars.find(*it) == std::string::npos)
//          break;
//      return str.substr(0, str.end() - it - 1);
//    }
//
//    bool starts_with(std::string_view const & str, std::string_view const & needle) {
//      return str.substr(0, needle.length()) == needle;
//    }
//
//    bool ends_with(std::string_view const & str, std::string_view const & needle) {
//      if (str.length() < needle.length())
//        return false;
//      else
//        return str.substr(str.length() - needle.length(), needle.length()) == needle;
//    }
//
//    std::vector<std::string_view> split(std::string_view const & str, std::vector<std::string> const & sep = whitespace) {
//      std::vector<std::string_view> result;
//
//      std::string_view remaining = str;
//
//      while (true) {
//        size_t nextToken = std::string::npos;
//        std::string_view found;
//        for (std::string const & needle : sep) {
//          size_t index = remaining.substr(0, nextToken).find_first_of(needle);
//          if (index == std::string::npos)
//            continue;
//          nextToken = index;
//          found     = remaining.substr(index, needle.size());
//        }
//
//        if (nextToken == std::string::npos)
//          break;
//
//        // Add up until `found`
//        if (nextToken > 0)
//          result.push_back(remaining.substr(0, nextToken));
//        // Skip to the end of `found`
//        remaining = remaining.substr(nextToken + found.size());
//      }
//
//      if (remaining.size() > 0)
//        result.push_back(remaining);
//
//      return result;
//    }
//  }
//
//  namespace compiler {
//    enum class symbol_type {
//      function,
//      variable,
//    };
//
//    enum class symbol_flags {
//      none         = 0,
//      const_       = 1 << 0,
//      extern_      = 1 << 1,
//      fn_parameter = 1 << 2, ///< This variable is a function parameter
//    };
//  }
//
//  template<>
//  struct enable_bitwise_ops<compiler::symbol_flags> : std::true_type {};
//}
//
//namespace adder {
//
//  namespace vm {
//    using register_value = uint64_t;
//    using register_index = uint8_t;
//
//    enum class op_code : uint64_t {
//      exit,             ///< Load a value from a memory address
//      load,             ///< Load a value from a memory address
//      load_stack,       ///< Load a value from the stack.
//      load_addr,        ///< Load constant address to a register.
//      store,            ///< Store a value to a memory address
//      store_stack,      ///< Store a value to the stack.
//      set,              ///< Set the value of a register
//      add_i64,          ///< Add two registers as integers
//      add_f64,          ///< Add two registers as floats
//      mul_i64,          ///< Multiply two registers as integers
//      mul_f64,          ///< Multiply two registers as floats
//      div_i64,          ///< Set the value of a register as integers
//      div_f64,          ///< Divide two registers as floats
//      push,             ///< Push a register to the stack
//      pop,              ///< Pop a register value from the stack. Store in named register
//      jump,             ///< Set the program counter.
//      move,             ///< Move a value from a new register
//      compare_i64,      ///< Compare the values in two registers as integers
//      compare_f64,      ///< Compare the values in two registers as floats
//      conditional_jump, ///< Set the program counter if the specified comparison bits are set.
//      conditional_move, ///< Compare the specified register with a value. Move if equal
//      count,            ///< Number of op codes
//    };
//
//    template<op_code code>
//    struct op_code_args {};
//
//    template<> struct op_code_args<op_code::exit> {};
//
//    template<> struct op_code_args<op_code::load> {
//      register_index dst;
//      register_index src_addr; // [reg]
//      uint8_t        size;
//    };
//
//    template<> struct op_code_args<op_code::load_stack> {
//      register_index dst;
//      uint8_t        size;
//      uint16_t       offset; // [reg]
//    };
//
//    template<> struct op_code_args<op_code::load_addr> {
//      register_index dst;
//      uint64_t       addr; // Constant address within the program. Must be relocated when the program is loaded.
//    };
//
//    template<> struct op_code_args<op_code::store> {
//      register_index src;
//      register_index dst_addr; // [reg]
//      uint8_t        size;
//    };
//
//    template<> struct op_code_args<op_code::store_stack> {
//      register_index src;
//      uint8_t        size;
//      uint16_t       offset; // [reg]
//    };
//
//    template<> struct op_code_args<op_code::set> {
//      register_index dst;
//      register_value val;
//    };
//
//    struct op_code_binary_op_args {
//      register_index dst;
//      register_index lhs;
//      register_index rhs;
//    };
//
//    template<> struct op_code_args<op_code::add_i64> : op_code_binary_op_args {};
//    template<> struct op_code_args<op_code::add_f64> : op_code_binary_op_args {};
//    template<> struct op_code_args<op_code::mul_i64> : op_code_binary_op_args {};
//    template<> struct op_code_args<op_code::mul_f64> : op_code_binary_op_args {};
//    template<> struct op_code_args<op_code::div_i64> : op_code_binary_op_args {};
//    template<> struct op_code_args<op_code::div_f64> : op_code_binary_op_args {};
//
//    template<> struct op_code_args<op_code::push> {
//      register_index src;
//      uint8_t        size;
//    };
//
//    template<> struct op_code_args<op_code::pop> {
//      register_index dst;
//      uint8_t        size;
//    };
//
//    template<> struct op_code_args<op_code::jump> {
//      uint64_t dst_addr;
//    };
//
//    template<> struct op_code_args<op_code::move> {
//      register_index dst;
//      register_index src;
//    };
//
//    inline static constexpr uint8_t cmp_lt_bit = 1 << 0;
//    inline static constexpr uint8_t cmp_eq_bit = 1 << 1;
//    inline static constexpr uint8_t cmp_gt_bit = 1 << 2;
//
//    template<> struct op_code_args<op_code::compare_i64> : op_code_binary_op_args {};
//    template<> struct op_code_args<op_code::compare_f64> : op_code_binary_op_args {};
//
//    template<> struct op_code_args<op_code::conditional_jump> : op_code_args<op_code::jump> {
//      register_index cmp_reg;
//      uint8_t        cmp_val;
//    };
//
//    template<> struct op_code_args<op_code::conditional_move> : op_code_args<op_code::move> {
//      register_index cmp_reg;
//      uint8_t        cmp_val;
//    };
//
//    inline static constexpr size_t op_code_count = (size_t)op_code::count;
//
//    namespace detail {
//      inline static constexpr size_t instruction_size[op_code_count] = {
//        sizeof(op_code) + sizeof(op_code_args<op_code::exit>),
//        sizeof(op_code) + sizeof(op_code_args<op_code::load>),
//        sizeof(op_code) + sizeof(op_code_args<op_code::load_stack>),
//        sizeof(op_code) + sizeof(op_code_args<op_code::load_addr>),
//        sizeof(op_code) + sizeof(op_code_args<op_code::store>),
//        sizeof(op_code) + sizeof(op_code_args<op_code::store_stack>),
//        sizeof(op_code) + sizeof(op_code_args<op_code::set>),
//        sizeof(op_code) + sizeof(op_code_args<op_code::add_i64>),
//        sizeof(op_code) + sizeof(op_code_args<op_code::add_f64>),
//        sizeof(op_code) + sizeof(op_code_args<op_code::mul_i64>),
//        sizeof(op_code) + sizeof(op_code_args<op_code::mul_f64>),
//        sizeof(op_code) + sizeof(op_code_args<op_code::div_i64>),
//        sizeof(op_code) + sizeof(op_code_args<op_code::div_f64>),
//        sizeof(op_code) + sizeof(op_code_args<op_code::push>),
//        sizeof(op_code) + sizeof(op_code_args<op_code::pop>),
//        sizeof(op_code) + sizeof(op_code_args<op_code::jump>),
//        sizeof(op_code) + sizeof(op_code_args<op_code::move>),
//        sizeof(op_code) + sizeof(op_code_args<op_code::compare_i64>),
//        sizeof(op_code) + sizeof(op_code_args<op_code::compare_f64>),
//        sizeof(op_code) + sizeof(op_code_args<op_code::conditional_jump>),
//        sizeof(op_code) + sizeof(op_code_args<op_code::conditional_move>)
//      };
//    }
//
//    inline static constexpr size_t instruction_size(op_code code) {
//      return detail::instruction_size[(int)code];
//    }
//
//    /// Variable number of arguments.
//    /// Dependent on op code implementation
//    struct instruction {
//      op_code  code;
//      union {
//        uint8_t arg_bytes[1];
//        op_code_args<op_code::load> load;
//        op_code_args<op_code::load_stack> load_stack;
//        op_code_args<op_code::load_addr> load_addr;
//        op_code_args<op_code::store> store;
//        op_code_args<op_code::store_stack> store_stack;
//        op_code_args<op_code::set> set;
//        op_code_binary_op_args add;
//        op_code_binary_op_args mul;
//        op_code_binary_op_args div;
//        op_code_args<op_code::push> push;
//        op_code_args<op_code::pop> pop;
//        op_code_args<op_code::jump> jump;
//        op_code_args<op_code::move> move;
//        op_code_binary_op_args compare;
//        op_code_args<op_code::conditional_jump> conditional_jump;
//        op_code_args<op_code::conditional_move> conditional_move;
//      };
//    };
//
//    struct allocator {
//      struct block {
//        uint64_t offset;
//        uint64_t size;
//      };
//
//      std::list<block>     blocks; ///< Available blocks
//      std::vector<uint8_t> data;   ///< Allocated data for this heap.
//
//      uint8_t const * read(uint64_t address) const {
//        return &data[address];
//      }
//
//      uint8_t * read(uint64_t address) {
//        return &data[address];
//      }
//
//      size_t write(uint64_t address, uint8_t const * bytes, size_t count) {
//        memcpy(&data[address], bytes, count);
//        return count;
//      }
//
//      uint64_t allocate(size_t size) {
//        uint64_t addr = data.size();
//        data.resize(data.size() + size);
//        return addr;
//      }
//
//      void free(uint64_t block) {
//        unused(block);
//        // Can't free yet.
//      }
//    };
//
//    struct memory {
//      inline static constexpr uint64_t stack_top = std::numeric_limits<uint64_t>::max();
//
//      /// Allocate heap memory.
//      uint64_t allocate(size_t size) {
//        return heap.allocate(size);
//      }
//
//      /// Free a heap allocation.
//      void free(uint64_t address) {
//        return heap.free(address);
//      }
//
//      /// Push data to the stack
//      uint64_t push(void const * data, size_t size) {
//        const size_t start = stack_bottom();
//        stack.insert(stack.end(), (uint8_t const *)data, (uint8_t const *)data + size);
//        return stack_top - start;
//      }
//
//      /// Pop data from the stack
//      void pop(size_t bytes) {
//        stack.erase(stack.end() - bytes, stack.end());
//      }
//
//      uint64_t stack_bottom() const {
//        return stack_top - stack.size();
//      }
//
//      bool is_stack(uint64_t address) const {
//        return address >= stack_bottom() && address < stack_top;
//      }
//
//      uint8_t * read(uint64_t address) {
//        if (!is_stack(address))
//          return heap.read(address);
//        else
//          return read_stack(address - stack_bottom());
//      }
//
//      uint8_t const * read(uint64_t address) const {
//        if (!is_stack(address))
//          return heap.read(address);
//        else
//          return read_stack(address - stack_bottom());
//      }
//
//      size_t write(uint64_t address, uint8_t const * bytes, size_t count) {
//        if (!is_stack(address))
//          return heap.write(address, bytes, count);
//        else
//          return write_stack(address - stack_bottom(), bytes, count);
//      }
//
//      uint8_t * read_stack(uint64_t offset) {
//        return &stack[offset];
//      }
//
//      uint8_t const * read_stack(uint64_t offset) const {
//        return &stack[offset];
//      }
//
//      size_t write_stack(uint64_t offset, uint8_t const * bytes, size_t count) {
//        memcpy(&stack[offset], bytes, count);
//        return count;
//      }
//
//      std::vector<uint8_t> stack; ///< Stack allocator
//      allocator            heap;  ///< Heap allocator
//    };
//
//    enum class register_names {
//      r0,
//      r1,
//      r2,
//      r3,
//      r4,
//      r5,
//      r6,
//      r7,
//      count
//    };
//    inline static constexpr size_t register_count = (size_t)register_names::count;
//
//    // TODO: Heap/Stack need to be behind a 'memory' interface
//    //       for unified address space.
//    struct machine {
//      register_value registers[register_count];
//      uint64_t       program_counter;  ///< Address of next instruction to execute
//      instruction    next_instruction;
//      memory         memory;
//    };
//
//    bool decode(machine * vm) {
//      instruction const * pInstruction = reinterpret_cast<instruction const *>(vm->memory.read(vm->program_counter));
//      size_t sz = instruction_size(pInstruction->code);
//      vm->program_counter += sz;
//      memcpy(&vm->next_instruction, pInstruction, sz);
//      return true;
//    }
//
//    void relocate_program(uint8_t * program, size_t size, uint64_t base) {
//      uint8_t * pc  = program;
//      uint8_t * end = program + size;
//
//      while (pc < end) {
//        vm::instruction *inst = (vm::instruction*)pc;
//        switch (inst->code)
//        {
//        case vm::op_code::load_addr:
//          inst->load_addr.addr += base;
//          break;
//        case vm::op_code::jump:
//        case vm::op_code::conditional_jump:
//          inst->jump.dst_addr  += base;
//          break;
//        }
//
//        pc += instruction_size(inst->code);
//      }
//    }
//
//    uint64_t load_program(vm::machine * vm, std::vector<uint8_t> const & program) {
//      // Reserve space for the program
//      uint64_t baseOffset = vm->memory.allocate(program.size());
//      // Write to memory
//      vm->memory.write(baseOffset, program.data(), program.size());
//      // Relocate the program to the loaded base address
//      relocate_program(vm->memory.read(baseOffset), program.size(), baseOffset);
//      return baseOffset;
//    }
//
//    namespace op {
//      void load(machine * vm, op_code_args<op_code::load> const & args) {
//        const uint64_t addr = vm->registers[args.src_addr];
//        const uint8_t *mem  = vm->memory.read(addr);
//
//        memcpy(&vm->registers[args.dst], mem, args.size);
//      }
//
//      void load_stack(machine * vm, op_code_args<op_code::load_stack> const & args) {
//        vm->registers[args.dst] = 0;
//
//        memcpy(&vm->registers[args.dst], vm->memory.read_stack(args.offset), args.size);
//      }
//
//      void load_addr(machine * vm, op_code_args<op_code::load_addr> const & args) {
//        vm->registers[args.dst] = args.addr;
//      }
//
//      void store(machine * vm, op_code_args<op_code::store> const & args) {
//        const uint64_t addr = vm->registers[args.dst_addr];
//        vm->memory.write(addr, reinterpret_cast<uint8_t const *>(&vm->registers[args.src]), args.size);
//      }
//
//      void store_stack(machine * vm, op_code_args<op_code::store_stack> const & args) {
//        vm->memory.write_stack(args.offset, reinterpret_cast<uint8_t const *>(&vm->registers[args.src]), args.size);
//      }
//
//      void set(machine * vm, op_code_args<op_code::set> const & args) {
//        vm->registers[args.dst] = args.val;
//      }
//
//      void add_i64(machine * vm, op_code_binary_op_args const & args) {
//        const int64_t lhs = (int64_t)vm->registers[args.lhs];
//        const int64_t rhs = (int64_t)vm->registers[args.rhs];
//        vm->registers[args.dst] = (uint64_t)(lhs + rhs);
//      }
//
//      void add_f64(machine * vm, op_code_binary_op_args const & args) {
//        const double lhs = *(double const *)&vm->registers[args.lhs];
//        const double rhs = *(double const *)&vm->registers[args.rhs];
//        const double result = lhs + rhs;
//        vm->registers[args.dst] = *(uint64_t const*)&result;
//      }
//
//      void mul_i64(machine * vm, op_code_binary_op_args const & args) {
//        const int64_t lhs = (int64_t)vm->registers[args.lhs];
//        const int64_t rhs = (int64_t)vm->registers[args.rhs];
//        vm->registers[args.dst] = (uint64_t)(lhs * rhs);
//      }
//
//      void mul_f64(machine * vm, op_code_binary_op_args const & args) {
//        const double lhs = *(double const *)&vm->registers[args.lhs];
//        const double rhs = *(double const *)&vm->registers[args.rhs];
//        const double result = lhs * rhs;
//        vm->registers[args.dst] = *(uint64_t const*)&result;
//      }
//
//      void div_i64(machine * vm, op_code_binary_op_args const & args) {
//        const int64_t lhs = (int64_t)vm->registers[args.lhs];
//        const int64_t rhs = (int64_t)vm->registers[args.rhs];
//        vm->registers[args.dst] = (uint64_t)(lhs / rhs);
//      }
//
//      void div_f64(machine * vm, op_code_binary_op_args const & args) {
//        const double lhs = *(double const *)&vm->registers[args.lhs];
//        const double rhs = *(double const *)&vm->registers[args.rhs];
//        const double result = lhs / rhs;
//        vm->registers[args.dst] = *(uint64_t const*)&result;
//      }
//
//      void push(machine * vm, op_code_args<op_code::push> const & args) {
//        vm->memory.push(&vm->registers[args.src], args.size);
//      }
//
//      void pop(machine * vm, op_code_args<op_code::pop> const & args) {
//        vm->memory.pop(args.size);
//      }
//
//      void jump(machine * vm, op_code_args<op_code::jump> const & args) {
//        vm->program_counter = args.dst_addr;
//      }
//
//      void move(machine * vm, op_code_args<op_code::move> const & args) {
//        vm->registers[args.dst] = vm->registers[args.src];
//      }
//
//      void compare_i64(machine * vm, op_code_binary_op_args const & args) {
//        const int64_t lhs = vm->registers[args.lhs];
//        const int64_t rhs = vm->registers[args.rhs];
//        uint64_t &dst = vm->registers[args.dst];
//        dst = 0;
//        if (lhs < rhs) dst |= cmp_lt_bit;
//        if (lhs == rhs) dst |= cmp_eq_bit;
//        if (lhs > rhs) dst |= cmp_gt_bit;
//      }
//
//      void compare_f64(machine * vm, op_code_binary_op_args const & args) {
//        const double lhs = *(const double *)&vm->registers[args.lhs];
//        const double rhs = *(const double *)&vm->registers[args.rhs];
//
//        uint64_t &dst = vm->registers[args.dst];
//        dst = 0;
//        if (lhs < rhs) dst |= cmp_lt_bit;
//        if (lhs == rhs) dst |= cmp_eq_bit;
//        if (lhs > rhs) dst |= cmp_gt_bit;
//      }
//
//      void conditional_jump(machine * vm, op_code_args<op_code::conditional_jump> const & args) {
//        if (args.cmp_val == (vm->registers[args.cmp_reg] & 0xFF))
//          jump(vm, args);
//      }
//
//      void conditional_move(machine * vm, op_code_args<op_code::conditional_move> const & args) {
//        if (args.cmp_val == (vm->registers[args.cmp_reg] & 0xFF))
//          move(vm, args);
//      }
//    }
//
//    bool execute(machine * vm) {
//      instruction const & inst = vm->next_instruction;
//      switch (inst.code) {
//      case op_code::load:
//        op::load(vm, inst.load);
//        break;
//      case op_code::load_stack:
//        op::load_stack(vm, inst.load_stack);
//        break;
//      case op_code::load_addr:
//        op::load_addr(vm, inst.load_addr);
//        break;
//      case op_code::store:
//        op::store(vm, inst.store);
//        break;
//      case op_code::store_stack:
//        op::store_stack(vm, inst.store_stack);
//        break;
//      case op_code::set:
//        op::set(vm, inst.set);
//        break;
//      case op_code::add_i64:
//        op::add_i64(vm, inst.add);
//        break;
//      case op_code::add_f64:
//        op::add_f64(vm, inst.add);
//        break;
//      case op_code::mul_i64:
//        op::mul_i64(vm, inst.add);
//        break;
//      case op_code::mul_f64:
//        op::mul_f64(vm, inst.add);
//        break;
//      case op_code::div_i64:
//        op::div_i64(vm, inst.add);
//        break;
//      case op_code::div_f64:
//        op::div_f64(vm, inst.add);
//        break;
//      case op_code::push:
//        op::push(vm, inst.push);
//        break;
//      case op_code::pop:
//        op::pop(vm, inst.pop);
//        break;
//      case op_code::jump:
//        op::jump(vm, inst.jump);
//        break;
//      case op_code::move:
//        op::move(vm, inst.move);
//        break;
//      case op_code::compare_i64:
//        op::compare_i64(vm, inst.compare);
//        break;
//      case op_code::compare_f64:
//        op::compare_f64(vm, inst.compare);
//        break;
//      case op_code::conditional_jump:
//        op::conditional_jump(vm, inst.conditional_jump);
//        break;
//      case op_code::conditional_move:
//        op::conditional_move(vm, inst.conditional_move);
//        break;
//      }
//
//      return inst.code != op_code::exit;
//    }
//
//    bool step(machine *vm) {
//      return decode(vm)
//        && execute(vm);
//    }
//  }
//
//  enum class symbol_type {
//    function,
//    variable,
//    type,
//  };
//
//  struct symbol {
//    symbol_type type;
//    std::string signature;
//  };
//
//  struct program_symbol_database {
//    std::vector<symbol> symbols;
//  };
//
//  // Compiled Program Layout
//  // header
//  //  * public_symbol_count: uint64_t
//  //  * extern_symbol_count: uint64_t
//  //  * symbol_data_size:    uint64_t
//  //  * program_data_size:   uint64_t
//  //  * code_size:           uint64_t
//  // public_symbol_table[]
//  // extern_symbol_table[]
//  // symbol_data
//  // program_data
//  // code
//  //
//  // public_symbols is a sequence of symbol address/data address pairs.
//  //   * symbol address is the location of the symbol name. Symbol name is a c-string
//  //   * data address is the location of the data. Format of the data depends on the type of symbol.
//  //     For a function, this is code.
//  //     For a variable, this is the value.
//  //   * public_symbols is terminated by a [ 0, 0 ] pair.
//  //
//  // extern_symbols is a sequence of address/symbol pairs.
//  //   * Same as public_symbols except data address is 0 at program load time.
//  //   * When the program is loaded, the vm should try resolve external symbols.
//  //   * The VM will query the host for the symbol addresses and write the resolved address to "data address" in the table.
//
//  struct program {
//    struct header {
//      uint32_t version              = 1;
//      uint32_t header_size          = sizeof(header);
//      uint64_t public_symbol_offset = 0;
//      uint64_t public_symbol_count  = 0;
//      uint64_t extern_symbol_offset = 0;
//      uint64_t extern_symbol_count  = 0;
//      uint64_t symbol_data_offset   = 0;
//      uint64_t symbol_data_size     = 0;
//      uint64_t program_data_offset  = 0;
//      uint64_t program_data_size    = 0;
//      uint64_t code_offset          = 0;
//      uint64_t code_size            = 0;
//    };
//
//    struct symbol_table_entry {
//      uint64_t name_address = 0;
//      uint64_t data_address = 0;
//    };
//
//    program() = default;
//    program(std::vector<uint8_t> binary) : binary(std::move(binary)) {}
//
//    header & get_header() {
//      return *(header*)&binary[0];
//    }
//
//    header const & get_header() const {
//      return *(header const *)&binary[0];
//    }
//
//    symbol_table_entry * get_public_symbols() {
//      uint8_t * table = &binary[0] + get_header().public_symbol_offset;
//      return (symbol_table_entry*)table;
//    }
//
//    symbol_table_entry const * get_public_symbols() const {
//      uint8_t const * table = &binary[0] + get_header().public_symbol_offset;
//      return (symbol_table_entry const *)table;
//    }
//
//    symbol_table_entry * get_extern_symbols() {
//      uint8_t * table = &binary[0] + get_header().extern_symbol_offset;
//      return (symbol_table_entry*)table;
//    }
//
//    symbol_table_entry const * get_extern_symbols() const {
//      uint8_t const * table = &binary[0] + get_header().extern_symbol_offset;
//      return (symbol_table_entry const *)table;
//    }
//
//    std::string_view get_symbol(symbol_table_entry const & entry) const {
//      return (char const *)&binary[entry.name_address];
//    }
//
//    std::vector<uint8_t> binary;
//  };
//
//  // Compiler implementation
//  namespace compiler {
//    template<vm::op_code op>
//    uint64_t add(std::vector<uint8_t> *program, vm::op_code_args<op> const & inst) {
//      constexpr size_t sz = instruction_size(op);
//
//      vm::instruction opaque;
//      opaque.code = op;
//      memcpy(opaque.arg_bytes, &inst, sizeof(inst));
//      uint8_t const * begin = (uint8_t const *)&opaque;
//      uint8_t const * end   = begin + sz;
//
//      uint64_t instructionAddress = program->size();
//      program->insert(program->end(), begin, end);
//      return instructionAddress;
//    }
//
//    // Function scope conventions.
//    // 1. Allocate all stack variables at top of scope.
//    // 2. Pop all stack variables at end of scope.
//
//    // Calling convention
//    // 1. Push used registers to stack.
//    // 2. Push pc to stack.
//    // 3. Push arguments to stack in order specified.
//    // 4. Jump to function address/entry point.
//    //      Execute function (reference arguments from stack). All registers are unused, so use registers as required.
//    //      Push return value to stack.
//    // 6. Pop/use return value.
//    // 7. Pop all arguments from stack.
//
//    // Wrap a block in the byte code for a function call.
//    // r0 reserved for return value.
//    // r1-register_count for parameters.
//    // std::vector<uint8_t> function_call(/* signature */) {
//    //   // Assumes all registers are unused
//    //   // Used registers should be pushed to the stack beforehand
//    // 
//    // }
//
//    // Handle consuming the result of a function
//    // std::vector<uint8_t> function_return() {
//    // 
//    // }
//
//    namespace lexer {
//      enum class token_class {
//        unknown = -1,
//        identifier,
//        literal,
//        operator_,
//        keyword,
//        grammar,
//        count,
//      };
//
//      enum class token_id {
//        none, // Identifier
//
//        // Keywords
//        fn,
//        init,
//        class_,
//        this_,
//        extern_,
//        as,
//        const_,
//        let,
//
//        if_,
//        else_,
//        elseif,
//
//        // Grammar
//        colon,
//        arrow,
//        semi_colon,
//        open_paren,
//        close_paren,
//        open_brace,
//        close_brace,
//        comma,
//        new_line,
//        line_comment,
//        open_block_comment,
//        close_block_comment,
//        quote,
//        eof,
//
//        // Operators
//        assign,
//        equal,
//        not_equal,
//        less_equal,
//        greater_equal,
//        less,
//        greater,
//        bang,
//        multiply,
//        divide,
//        add,
//        minus,
//        dot,
//        call,
//
//        count
//      };
//      inline static constexpr size_t token_id_count = (size_t)token_id::count;
//
//      struct token_desc {
//        std::string name;
//        token_id id;
//        token_class cls;
//      };
//
//      inline static std::vector<token_desc> tokens = {
//        { "fn",     token_id::fn,                  token_class::keyword },
//        { "init",   token_id::init,                token_class::keyword },
//        { "this",   token_id::this_,               token_class::keyword },
//        { "class",  token_id::class_,               token_class::keyword },
//        { "extern", token_id::extern_,             token_class::keyword },
//        { "as",     token_id::as,                  token_class::keyword },
//        { "const",  token_id::const_,              token_class::keyword },
//        { "let",    token_id::let,                 token_class::keyword },
//        { ":",      token_id::colon,               token_class::grammar},
//        { "=>",     token_id::arrow,               token_class::grammar},
//        { ";",      token_id::semi_colon,          token_class::grammar},
//        { "(",      token_id::open_paren,          token_class::grammar},
//        { ")",      token_id::close_paren,         token_class::grammar},
//        { "{",      token_id::open_brace,          token_class::grammar},
//        { "}",      token_id::close_brace,         token_class::grammar},
//        { ",",      token_id::comma,               token_class::grammar},
//        { "\n",     token_id::new_line,            token_class::grammar},
//        { "//",     token_id::line_comment,        token_class::grammar},
//        { "/*",     token_id::open_block_comment,  token_class::grammar},
//        { "*/",     token_id::close_block_comment, token_class::grammar},
//        { "\"",     token_id::quote,               token_class::grammar},
//        // { "", token_id::eof,           token_class::grammar}, // eof token is not parsed. it is appended to the input
//        { "=",      token_id::assign,              token_class::operator_ },
//        { "==",     token_id::equal,               token_class::operator_ },
//        { "!=",     token_id::not_equal,           token_class::operator_ },
//        { "<=",     token_id::less_equal,          token_class::operator_ },
//        { ">=",     token_id::greater_equal,       token_class::operator_ },
//        { "<",      token_id::less,                token_class::operator_ },
//        { ">",      token_id::greater,             token_class::operator_ },
//        { "!",      token_id::bang,                token_class::operator_ },
//        { "*",      token_id::multiply,            token_class::operator_ },
//        { "/",      token_id::divide,              token_class::operator_ },
//        { "+",      token_id::add,                 token_class::operator_ },
//        { "-",      token_id::minus,               token_class::operator_ },
//        { ".",      token_id::dot,                 token_class::operator_ },
//      };
//
//      // Sorted by token length. When tokenizing, we search for the longest token first.
//      // This is to avoid conflicting with longer tokens including shorted tokens (e.g. '>' and '>=')
//      inline static const std::vector<token_desc> sorted_tokens = []() {
//        auto sorted = tokens;
//        std::sort(sorted.begin(), sorted.end(), [](token_desc const & a, token_desc const & b) {
//          return a.name.length() > b.name.length();
//        });
//        return sorted;
//      }();
//
//      inline static const std::string token_terminators = []() {
//        std::string chars;
//        chars = " \r\n\t\v";
//        for (auto & token : tokens)
//          if (token.cls == token_class::operator_ || token.cls == token_class::grammar)
//            for (char c : token.name)
//              if (chars.find(c) == std::string::npos)
//                chars += c;
//        return chars;
//      }();
//
//      struct token_view {
//        token_id id;
//        token_class cls;
//        std::string_view name;
//      };
//
//      class token_parser {
//      public:
//        token_parser() = default;
//        token_parser(std::string _source)
//          : m_source(std::move(_source)) {
//          m_remaining = m_source;
//          m_remaining = str::trim_start(m_remaining, " \r\t\v");
//
//          next();
//        }
//
//        token_parser& parse(token_id const & id, token_view * storage = nullptr) {
//          return parse([=](token_view const & token) -> bool { return token.id == id; }, storage);
//        }
//
//        token_parser& parse(token_class const & cls, token_view * storage = nullptr) {
//          return parse([=](token_view const & token) -> bool { return token.cls == cls; }, storage);
//        }
//
//        template<typename Rule>
//        token_parser& parse(Rule const & rule, token_view * storage = nullptr) {
//          if (!rule(current())) {
//            raise("Unexpected token " + std::string(current().name));
//            return *this;
//          }
//
//          if (storage != nullptr)
//            *storage = current();
//
//          if (!next()) {
//            raise("Unexpected eof reached");
//          }
//
//          return *this;
//        }
//
//        /// true if no errors have occured.
//        bool ok() const {
//          return m_errors.size() == 0;
//        }
//
//        /// Parse until the next token is found.
//        /// Currently parses out comments.
//        bool next() {
//          if (m_remaining.length() == 0) {
//            if (m_current.id == token_id::eof)
//              return false;
//            m_current.id = token_id::eof;
//            m_current.name = "";
//            m_current.cls = token_class::grammar;
//          }
//
//          std::optional<token_desc> candidate;
//          
//          for (auto const & token : sorted_tokens) {
//            if (str::starts_with(m_remaining, token.name)) {
//              candidate = token;
//              break;
//            }
//          }
//
//          size_t end = candidate.has_value() ? candidate->name.length() : 0;
//          if (!candidate.has_value() || (candidate->cls != token_class::grammar && candidate->cls != token_class::operator_))
//            end = m_remaining.find_first_of(token_terminators, end);
//
//          std::string_view nextToken = m_remaining.substr(0, end);
//          if (!candidate.has_value() || end != candidate->name.length()) {
//            m_current.id  = token_id::none;
//            m_current.cls = token_class::unknown;
//          }
//          else {
//            m_current.id   = candidate->id;
//            m_current.cls  = candidate->cls;
//          }
//          m_current.name = m_remaining.substr(0, end);
//          m_remaining    = str::trim_start(m_remaining.substr(end), " \r\t\v"); // Skip whitespace (except \n as we tokenize new lines)
//
//          if (current().id == token_id::open_block_comment)
//            return parseBlockComment();
//          if (current().id == token_id::line_comment)
//            return parseLineComment();
//          return true;
//        }
//
//        token_view const & current() const {
//          return m_current;
//        }
//
//        bool eof() const {
//          return current().id == token_id::eof;
//        }
//
//      private:
//        void raise(std::string const & error) {
//          m_errors.push_back(error);
//        }
//
//        bool parseLineComment() {
//          while (next() && current().id != token_id::new_line);
//          if (current().id != token_id::eof)
//            next();
//          return true;
//        }
//
//        bool parseBlockComment() {
//          while (next())
//          {
//            if (current().id == token_id::close_block_comment)
//              return true;
//
//            if (current().id == token_id::open_block_comment)
//              if (!parseBlockComment())
//                return false;
//          }
//
//          return false;
//        }
//
//        std::string      m_source;
//        std::string_view m_remaining;
//        token_view       m_current;
//
//        std::vector<std::string> m_errors;
//      };
//    }
//
//    namespace rules {
//      struct token_rule {
//        token_rule(std::function<bool(lexer::token_view const &)> const & func)
//          : pred(func)
//        {}
//
//        token_rule(lexer::token_id id) : pred([=](lexer::token_view const & token) { return token.id == id; }) {}
//        token_rule(lexer::token_class cls) : pred([=](lexer::token_view const & token) { return token.cls == cls; }) {}
//
//        bool operator()(lexer::token_view const & token) const {
//          return pred(token);
//        }
//
//        std::function<bool(lexer::token_view const &)>  pred;
//      };
//
//
//      token_rule or(token_rule const & a, token_rule const & b) {
//        return token_rule([=](lexer::token_view const & token) { return a(token) || b(token); });
//      }
//
//      token_rule xor(token_rule const & a, token_rule const & b) {
//        return token_rule([=](lexer::token_view const & token) { return a(token) ^ b(token); });
//      }
//
//      token_rule and(token_rule const & a, token_rule const & b) {
//        return token_rule([=](lexer::token_view const & token) { return a(token) && b(token); });
//      }
//    }
//
//    struct type_desc {
//      bool   builtin; ///< 
//      size_t size;    ///< Size of the type
//    };
//
//
//    struct statement {
//      statement() = default;
//      statement(lexer::token_view token) : token(token) {};
//
//      lexer::token_view token;
//
//      std::unique_ptr<statement> left  = nullptr;
//      std::unique_ptr<statement> right = nullptr;
//    };
//
//    struct symbol_desc {
//      symbol_type  symbol_type;
//      std::string  identifier;
//      std::string  signature;
//      symbol_flags flags;
//    };
//
//    symbol_desc variable_symbol(std::string_view const & identifier, std::string_view const & type, symbol_flags flags = symbol_flags::none) {
//      return {
//        symbol_type::variable,
//        std::string(identifier),
//        std::string(type),
//        flags
//      };
//    }
//    symbol_desc function_symbol(std::string_view const & identifier, std::string_view const & signature, symbol_flags flags = symbol_flags::none) {
//      return {
//        symbol_type::function,
//        std::string(identifier),
//        std::string(signature),
//        flags
//      };
//    }
//
//    struct context {
//      // Pointer to the parent scope.
//      // Allows us to walk up the tree.
//      context * parent_scope;
//      // Name of this scope. Used to augment local public symbol names
//      std::string scope_name = "";
//      // Types that have been parsed
//      std::map<std::string, type_desc> types;
//      // Symbols in the current scope
//      std::vector<symbol_desc> symbols;
//      // Function bodies
//      std::map<std::string, std::unique_ptr<context>> functions;
//      // Statements in this scope in sequential order.
//      std::vector<
//        std::variant<
//          std::unique_ptr<statement>,
//          std::unique_ptr<context>
//        >> statements;
//
//      // Statement currently being parsed
//      std::unique_ptr<statement> currentStatement;
//
//      // Return type for this scope.
//      std::optional<std::string> return_type;
//    };
//
//    symbol_desc * find_symbol(context * state, std::string_view const & identifier) {
//      for (auto & desc : state->symbols) {
//        if (desc.identifier == identifier) {
//          return &desc;
//        }
//      }
//      return nullptr;
//    }
//
//    bool consume_code(context * state, lexer::token_parser * tokenizer);
//    bool consume_fn(context * state, lexer::token_parser * tokenizer, symbol_flags flags = symbol_flags::none);
//    bool consume_const(context * state, lexer::token_parser * tokenizer);
//    bool consume_let(context * state, lexer::token_parser * tokenizer);
//    bool consume_class(context * state, lexer::token_parser * tokenizer);
//    bool consume_extern(context * state, lexer::token_parser * tokenizer);
//
//    bool consume_block(context * state, lexer::token_parser * tokenizer) {
//      if (!tokenizer->parse(lexer::token_id::open_brace).ok())
//        return false;
//
//      context block;
//      block.parent_scope = state;
//      block.scope_name   = state->scope_name + "$" + std::to_string(state->statements.size() + 1);
//
//      while (tokenizer->current().id != lexer::token_id::close_brace)
//        consume_code(&block, tokenizer);
//
//      if (tokenizer->eof())
//        return false;
//
//      state->statements.push_back(std::make_unique<context>(std::move(block)));
//
//      return true;
//    }
//    
//    bool consume_parameter(context * state, lexer::token_parser * tokenizer) {
//      lexer::token_view identifier = tokenizer->current();
//      lexer::token_view type = tokenizer->current();
//      if (!tokenizer->
//         parse(lexer::token_id::none, &identifier)
//        .parse(lexer::token_id::colon)
//        .parse(lexer::token_id::none, &type)
//        .ok())
//        return false;
//
//      state->symbols.push_back(variable_symbol(identifier.name, type.name, symbol_flags::fn_parameter));
//
//      return true;
//    }
//
//    bool takes_precedence(lexer::token_id op, lexer::token_id over) {
//      static const std::vector<lexer::token_id> precedence = {
//        lexer::token_id::assign,
//        lexer::token_id::equal,
//        lexer::token_id::not_equal,
//        lexer::token_id::greater_equal,
//        lexer::token_id::greater,
//        lexer::token_id::less_equal,
//        lexer::token_id::less,
//        lexer::token_id::add,
//        lexer::token_id::minus,
//        lexer::token_id::divide,
//        lexer::token_id::multiply,
//        lexer::token_id::bang,
//        lexer::token_id::call,
//        lexer::token_id::dot,
//        lexer::token_id::none, // Evaluate identifiers and literals first
//      };
//
//      return std::find(precedence.begin(), precedence.end(), op) > std::find(precedence.begin(), precedence.end(), over);
//    }
//
//    // Assumes newStatement takes precedence over 'parent'
//    bool insert_statement(std::unique_ptr<statement> const & parent, std::unique_ptr<statement> &&newStatement) {
//      if (parent->right != nullptr && takes_precedence(newStatement->token.id, parent->right->token.id)) {
//        return insert_statement(parent->right, std::move(newStatement));
//      }
//      else {
//        auto oldRight = std::move(parent->right);
//        parent->right = std::move(newStatement);
//
//        // newStatement is always to the right of any statements in `parent`.
//        // insert oldRight to the left of newStatement.
//        parent->right->left = std::move(oldRight);
//      }
//
//      return true;
//    }
//
//    bool insert_statement(context * state, std::unique_ptr<statement> &&newStatement) {
//      if (state->currentStatement == nullptr) {
//        state->currentStatement = std::move(newStatement);
//        return true;
//      }
//
//      if (takes_precedence(state->currentStatement->token.id, newStatement->token.id)) {
//        // Make next statement top of tree
//        newStatement->left = std::move(state->currentStatement);
//        state->currentStatement = std::move(newStatement);
//        return true;
//      }
//      else {
//        return insert_statement(state->currentStatement, std::move(newStatement));
//      }
//    }
//
//    bool insert_statement(context * state, lexer::token_view token) {
//      return insert_statement(state, std::make_unique<statement>(token));
//    }
//
//    std::unique_ptr<statement> consume_call(context * state, lexer::token_parser * tokenizer) {
//      // TODO: Parse argument list into subnodes of the 'call' statement
//      unused(state, tokenizer);
//
//      lexer::token_view callToken;
//      callToken.id = lexer::token_id::call;
//      callToken.cls = lexer::token_class::operator_;
//
//      return std::make_unique<statement>(callToken);
//    }
//
//    bool consume_statement(context * state, lexer::token_parser * tokenizer) {
//      lexer::token_view token;
//      if (!tokenizer->parse(
//        rules:: or (
//          rules:: or (
//            lexer::token_id::open_brace,
//            lexer::token_class::operator_
//            ),
//          rules::or(
//            lexer::token_id::none,
//            lexer::token_id::semi_colon
//            )
//          ), &token)
//        .ok())
//        return false;
//
//      if (token.id == lexer::token_id::semi_colon)
//      {
//        if (state->currentStatement != nullptr)
//          state->statements.push_back(std::move(state->currentStatement));
//        return true;
//      }
//
//      if (token.id == lexer::token_id::open_brace) {
//        insert_statement(state, consume_call(state, tokenizer));
//      }
//      else {
//        insert_statement(state, token);
//      }
//
//      return true;
//    }
//
//    bool consume_code(context * state, lexer::token_parser * tokenizer) {
//      switch (tokenizer->current().id) {
//      case lexer::token_id::fn:         return consume_fn(state, tokenizer);
//      case lexer::token_id::const_:     return consume_const(state, tokenizer);
//      case lexer::token_id::let:        return consume_let(state, tokenizer);
//      case lexer::token_id::class_:     return consume_class(state, tokenizer);
//      case lexer::token_id::extern_:    return consume_extern(state, tokenizer);
//      case lexer::token_id::open_brace: return consume_block(state, tokenizer);
//      // case lexer::token_id::if_:        return consume_if(state, tokenizer);
//      case lexer::token_id::new_line:   return tokenizer->next();
//      default:
//        return consume_statement(state, tokenizer);
//      }
//    }
//
//    bool consume_fn(context * state, lexer::token_parser * tokenizer, symbol_flags flags) {
//      lexer::token_view name;
//      if (!tokenizer->
//        parse(lexer::token_id::fn)
//        .parse(lexer::token_id::none, &name)
//        .parse(lexer::token_id::open_paren)
//        .ok())
//        return false;
//
//      context functionScope;
//      functionScope.parent_scope = state;
//      functionScope.types = state->types;
//      functionScope.scope_name = state->scope_name + "$" + std::string(name.name);
//
//      while (tokenizer->current().id != lexer::token_id::close_paren) {
//        consume_parameter(&functionScope, tokenizer);
//
//        if (tokenizer->current().id == lexer::token_id::close_paren)
//          break;
//        if (tokenizer->current().id != lexer::token_id::comma)
//          return false;
//        tokenizer->next();
//      }
//      tokenizer->next();
//
//      lexer::token_view returnType;
//      if (!tokenizer->
//        parse(lexer::token_id::arrow).
//        parse(lexer::token_id::none, &returnType).
//        ok())
//        return false;
//
//      functionScope.return_type = returnType.name;
//      std::string signature = "(";
//      size_t numParams = 0;
//      for (auto & symbol : functionScope.symbols) {
//        if ((symbol.flags & symbol_flags::fn_parameter) == symbol_flags::fn_parameter) {
//          if (numParams != 0)
//            signature += ",";
//          signature += symbol.signature;
//          ++numParams;
//        }
//      }
//      signature += ")=>" + std::string(returnType.name);
//
//      lexer::token_view token;
//      if (!tokenizer->
//        parse(rules::or(
//          lexer::token_id::semi_colon,
//          lexer::token_id::open_brace), &token)
//        .ok())
//        return false;
//
//      if (token.id == lexer::token_id::semi_colon)
//        return true;
//
//      // Parse function body.
//      while (tokenizer->current().id != lexer::token_id::close_brace)
//        consume_code(&functionScope, tokenizer);
//
//      std::string fullName(name.name);
//      fullName += signature;
//
//      state->symbols.push_back(function_symbol(name.name, signature, flags));
//      state->functions[fullName] = std::make_unique<context>(std::move(functionScope));
//
//      return !tokenizer->eof();
//    }
//
//    bool consume_const(context * state, lexer::token_parser * tokenizer) {
//      lexer::token_view identifier;
//      lexer::token_view type;
//      if (!tokenizer->
//        parse(lexer::token_id::const_).
//        parse(lexer::token_id::none, &identifier).
//        parse(lexer::token_id::colon).
//        parse(lexer::token_id::none, &type).
//        ok())
//        return false;
//
//      state->symbols.push_back(variable_symbol(identifier.name, type.name, symbol_flags::const_));
//      state->currentStatement = std::make_unique<statement>(identifier);
//
//      return consume_statement(state, tokenizer);
//    }
//
//    bool consume_let(context * state, lexer::token_parser * tokenizer) {
//      lexer::token_view identifier;
//      lexer::token_view type;
//      if (!tokenizer->
//        parse(lexer::token_id::let).
//        parse(lexer::token_id::none, &identifier).
//        parse(lexer::token_id::colon).
//        parse(lexer::token_id::none, &type).
//        ok())
//        return false;
//
//      state->symbols.push_back(variable_symbol(identifier.name, type.name));
//      state->currentStatement = std::make_unique<statement>(identifier);
//
//      return consume_statement(state, tokenizer);
//    }
//
//    bool consume_class(context * state, lexer::token_parser * tokenizer) {
//      unused(state, tokenizer);
//      return tokenizer->next();
//    }
//
//    bool consume_extern(context * state, lexer::token_parser * tokenizer) {
//      if (!tokenizer->parse(lexer::token_id::extern_).ok())
//        return false;
//
//      if (tokenizer->current().id == lexer::token_id::fn) {
//        return consume_fn(state, tokenizer, symbol_flags::extern_);
//      }
//      unused(state, tokenizer);
//      return tokenizer->next();
//    }
//
//    // Consume top level
//    bool consume_top_level_statement(context * state, lexer::token_parser * tokenizer) {
//      switch (tokenizer->current().id) {
//      case lexer::token_id::fn:       return consume_fn(state, tokenizer);
//      case lexer::token_id::const_:   return consume_const(state, tokenizer);
//      case lexer::token_id::let:      return consume_let(state, tokenizer);
//      case lexer::token_id::class_:   return consume_class(state, tokenizer);
//      case lexer::token_id::extern_:  return consume_extern(state, tokenizer);
//      case lexer::token_id::new_line: return tokenizer->next();
//      }
//
//      return false;
//    }
//
//    program compile(std::string const & source) {
//      lexer::token_parser tokenizer(source);
//
//      context root_scope;
//      while (consume_top_level_statement(&root_scope, &tokenizer)) {
//      }
//
//      return {};
//    }
//  }
//}

#include "vm.h"
#include "compiler.h"
#include "common.h"

#include <iostream>

int main(int argc, char ** argv) {
  adder::unused(argc, argv);

  std::string content;
  {
    FILE * file = fopen("spec.ad", "r");
    while (!feof(file)) {
      char buffer[1024];
      size_t numRead = fread(buffer, 1, sizeof(buffer), file);
      content += std::string_view(buffer, numRead);
    }
    fclose(file);
  }

  auto result = adder::compiler::compile(content);

  adder::vm::machine vm;
  uint64_t base  = adder::vm::load_program(&vm, result.binary);
  uint64_t entry = 0;
  for (uint64_t i = 0; i < result.get_header().public_symbol_count; ++i) {
    if (result.get_symbol(result.get_public_symbols()[i]) == "main") {
      entry = base + result.get_public_symbols()[i].data_address;
      break;
    }
  }

  // Set program counter to the entry point.
  vm.program_counter = entry;

  do
  {
    int i = 0;
    std::cout << "pc: " << vm.program_counter << std::endl;
    for (auto &reg : vm.registers)
      std::cout << "r" << i++ << ": " << reg << std::endl;

    adder::vm::decode(&vm);

    // TODO: print instruction details

  } while (adder::vm::execute(&vm));

  return 0;
}
