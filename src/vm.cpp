#include "vm.h"
#include "common.h"

namespace adder {
  namespace vm {
    namespace detail {
      // inline static constexpr size_t instruction_size[op_code_count] = {
      //   sizeof(op_code) + sizeof(op_code_args<op_code::exit>),
      //   sizeof(op_code) + sizeof(op_code_args<op_code::load>),
      //   sizeof(op_code) + sizeof(op_code_args<op_code::load_stack>),
      //   sizeof(op_code) + sizeof(op_code_args<op_code::load_addr>),
      //   sizeof(op_code) + sizeof(op_code_args<op_code::store>),
      //   sizeof(op_code) + sizeof(op_code_args<op_code::store_stack>),
      //   sizeof(op_code) + sizeof(op_code_args<op_code::set>),
      //   sizeof(op_code) + sizeof(op_code_args<op_code::add_i64>),
      //   sizeof(op_code) + sizeof(op_code_args<op_code::add_f64>),
      //   sizeof(op_code) + sizeof(op_code_args<op_code::mul_i64>),
      //   sizeof(op_code) + sizeof(op_code_args<op_code::mul_f64>),
      //   sizeof(op_code) + sizeof(op_code_args<op_code::div_i64>),
      //   sizeof(op_code) + sizeof(op_code_args<op_code::div_f64>),
      //   sizeof(op_code) + sizeof(op_code_args<op_code::push>),
      //   sizeof(op_code) + sizeof(op_code_args<op_code::pop>),
      //   sizeof(op_code) + sizeof(op_code_args<op_code::jump>),
      //   sizeof(op_code) + sizeof(op_code_args<op_code::move>),
      //   sizeof(op_code) + sizeof(op_code_args<op_code::compare_i64>),
      //   sizeof(op_code) + sizeof(op_code_args<op_code::compare_f64>),
      //   sizeof(op_code) + sizeof(op_code_args<op_code::conditional_jump>),
      //   sizeof(op_code) + sizeof(op_code_args<op_code::conditional_move>)
      // };
    }

    size_t instruction_size(op_code code) {
      return sizeof(instruction)/*detail::instruction_size[(int)code]*/;
    }

    uint8_t const * allocator::read(uint64_t address) const {
      return &data[address];
    }

    uint8_t * allocator::read(uint64_t address) {
      return &data[address];
    }

    size_t allocator::write(uint64_t address, uint8_t const * bytes, size_t count) {
      memcpy(&data[address], bytes, count);
      return count;
    }

    uint64_t allocator::allocate(size_t size) {
      uint64_t addr = data.size();
      data.resize(data.size() + size);
      return addr;
    }

    void allocator::free(uint64_t block) {
      unused(block);
      // Can't free yet.
    }

    uint64_t memory::allocate(size_t size) {
      return heap.allocate(size);
    }

    void memory::free(uint64_t address) {
      return heap.free(address);
    }

    uint64_t memory::push(void const * data, size_t size) {
      const size_t start = stack_bottom();
      stack.insert(stack.end(), (uint8_t const *)data, (uint8_t const *)data + size);
      return stack_top - start;
    }

    void memory::pop(size_t bytes) {
      stack.erase(stack.end() - bytes, stack.end());
    }

    uint64_t memory::stack_bottom() const {
      return stack_top - stack.size();
    }

    bool memory::is_stack(uint64_t address) const {
      return address >= stack_bottom() && address < stack_top;
    }

    uint8_t * memory::read(uint64_t address) {
      if (!is_stack(address))
        return heap.read(address);
      else
        return read_stack(address - stack_bottom());
    }

    uint8_t const * memory::read(uint64_t address) const {
      if (!is_stack(address))
        return heap.read(address);
      else
        return read_stack(address - stack_bottom());
    }

    size_t memory::write(uint64_t address, uint8_t const * bytes, size_t count) {
      if (!is_stack(address))
        return heap.write(address, bytes, count);
      else
        return write_stack(address - stack_bottom(), bytes, count);
    }

    uint8_t * memory::read_stack(uint64_t offset) {
      return &stack[offset];
    }

    uint8_t const * memory::read_stack(uint64_t offset) const {
      return &stack[offset];
    }

    size_t memory::write_stack(uint64_t offset, uint8_t const * bytes, size_t count) {
      memcpy(&stack[offset], bytes, count);
      return count;
    }

    bool decode(machine * vm) {
      instruction const * pInstruction = reinterpret_cast<instruction const *>(vm->memory.read(vm->program_counter));
      size_t sz = instruction_size(pInstruction->code);
      vm->program_counter += sz;
      memcpy(&vm->next_instruction, pInstruction, sz);
      return true;
    }

    void relocate_program(uint8_t * program, size_t size, uint64_t base) {
      uint8_t * pc  = program;
      uint8_t * end = program + size;

      while (pc < end) {
        vm::instruction *inst = (vm::instruction*)pc;
        switch (inst->code)
        {
        case vm::op_code::load_addr:
          inst->load_addr.addr += base;
          break;
        case vm::op_code::jump:
        case vm::op_code::conditional_jump:
          inst->jump.dst_addr  += base;
          break;
        }

        pc += instruction_size(inst->code);
      }
    }

    uint64_t load_program(vm::machine * vm, std::vector<uint8_t> const & program) {
      // Reserve space for the program
      uint64_t baseOffset = vm->memory.allocate(program.size());
      // Write to memory
      vm->memory.write(baseOffset, program.data(), program.size());
      // Relocate the program to the loaded base address
      relocate_program(vm->memory.read(baseOffset), program.size(), baseOffset);
      return baseOffset;
    }

    namespace op {
      void load(machine * vm, op_code_args<op_code::load> const & args) {
        const uint64_t addr = vm->registers[args.src_addr];
        const uint8_t *mem  = vm->memory.read(addr);

        memcpy(&vm->registers[args.dst], mem, args.size);
      }

      void load_stack(machine * vm, op_code_args<op_code::load_stack> const & args) {
        vm->registers[args.dst] = 0;

        memcpy(&vm->registers[args.dst], vm->memory.read_stack(args.offset), args.size);
      }

      void load_addr(machine * vm, op_code_args<op_code::load_addr> const & args) {
        vm->registers[args.dst] = args.addr;
      }

      void store(machine * vm, op_code_args<op_code::store> const & args) {
        const uint64_t addr = vm->registers[args.dst_addr];
        vm->memory.write(addr, reinterpret_cast<uint8_t const *>(&vm->registers[args.src]), args.size);
      }

      void store_stack(machine * vm, op_code_args<op_code::store_stack> const & args) {
        vm->memory.write_stack(args.offset, reinterpret_cast<uint8_t const *>(&vm->registers[args.src]), args.size);
      }

      void set(machine * vm, op_code_args<op_code::set> const & args) {
        vm->registers[args.dst] = args.val;
      }

      void add_i64(machine * vm, op_code_binary_op_args const & args) {
        const int64_t lhs = (int64_t)vm->registers[args.lhs];
        const int64_t rhs = (int64_t)vm->registers[args.rhs];
        vm->registers[args.dst] = (uint64_t)(lhs + rhs);
      }

      void add_f64(machine * vm, op_code_binary_op_args const & args) {
        const double lhs = *(double const *)&vm->registers[args.lhs];
        const double rhs = *(double const *)&vm->registers[args.rhs];
        const double result = lhs + rhs;
        vm->registers[args.dst] = *(uint64_t const*)&result;
      }

      void mul_i64(machine * vm, op_code_binary_op_args const & args) {
        const int64_t lhs = (int64_t)vm->registers[args.lhs];
        const int64_t rhs = (int64_t)vm->registers[args.rhs];
        vm->registers[args.dst] = (uint64_t)(lhs * rhs);
      }

      void mul_f64(machine * vm, op_code_binary_op_args const & args) {
        const double lhs = *(double const *)&vm->registers[args.lhs];
        const double rhs = *(double const *)&vm->registers[args.rhs];
        const double result = lhs * rhs;
        vm->registers[args.dst] = *(uint64_t const*)&result;
      }

      void div_i64(machine * vm, op_code_binary_op_args const & args) {
        const int64_t lhs = (int64_t)vm->registers[args.lhs];
        const int64_t rhs = (int64_t)vm->registers[args.rhs];
        vm->registers[args.dst] = (uint64_t)(lhs / rhs);
      }

      void div_f64(machine * vm, op_code_binary_op_args const & args) {
        const double lhs = *(double const *)&vm->registers[args.lhs];
        const double rhs = *(double const *)&vm->registers[args.rhs];
        const double result = lhs / rhs;
        vm->registers[args.dst] = *(uint64_t const*)&result;
      }

      void push(machine * vm, op_code_args<op_code::push> const & args) {
        vm->memory.push(&vm->registers[args.src], args.size);
      }

      void pop(machine * vm, op_code_args<op_code::pop> const & args) {
        vm->memory.pop(args.size);
      }

      void jump(machine * vm, op_code_args<op_code::jump> const & args) {
        vm->program_counter = args.dst_addr;
      }

      void move(machine * vm, op_code_args<op_code::move> const & args) {
        vm->registers[args.dst] = vm->registers[args.src];
      }

      void compare_i64(machine * vm, op_code_binary_op_args const & args) {
        const int64_t lhs = vm->registers[args.lhs];
        const int64_t rhs = vm->registers[args.rhs];
        uint64_t &dst = vm->registers[args.dst];
        dst = 0;
        if (lhs < rhs) dst |= cmp_lt_bit;
        if (lhs == rhs) dst |= cmp_eq_bit;
        if (lhs > rhs) dst |= cmp_gt_bit;
      }

      void compare_f64(machine * vm, op_code_binary_op_args const & args) {
        const double lhs = *(const double *)&vm->registers[args.lhs];
        const double rhs = *(const double *)&vm->registers[args.rhs];

        uint64_t &dst = vm->registers[args.dst];
        dst = 0;
        if (lhs < rhs) dst |= cmp_lt_bit;
        if (lhs == rhs) dst |= cmp_eq_bit;
        if (lhs > rhs) dst |= cmp_gt_bit;
      }

      void conditional_jump(machine * vm, op_code_args<op_code::conditional_jump> const & args) {
        if (args.cmp_val == (vm->registers[args.cmp_reg] & 0xFF))
          jump(vm, args);
      }

      void conditional_move(machine * vm, op_code_args<op_code::conditional_move> const & args) {
        if (args.cmp_val == (vm->registers[args.cmp_reg] & 0xFF))
          move(vm, args);
      }
    }

    bool execute(machine * vm) {
      instruction const & inst = vm->next_instruction;
      switch (inst.code) {
      case op_code::load:
        op::load(vm, inst.load);
        break;
      case op_code::load_stack:
        op::load_stack(vm, inst.load_stack);
        break;
      case op_code::load_addr:
        op::load_addr(vm, inst.load_addr);
        break;
      case op_code::store:
        op::store(vm, inst.store);
        break;
      case op_code::store_stack:
        op::store_stack(vm, inst.store_stack);
        break;
      case op_code::set:
        op::set(vm, inst.set);
        break;
      case op_code::add_i64:
        op::add_i64(vm, inst.add);
        break;
      case op_code::add_f64:
        op::add_f64(vm, inst.add);
        break;
      case op_code::mul_i64:
        op::mul_i64(vm, inst.add);
        break;
      case op_code::mul_f64:
        op::mul_f64(vm, inst.add);
        break;
      case op_code::div_i64:
        op::div_i64(vm, inst.add);
        break;
      case op_code::div_f64:
        op::div_f64(vm, inst.add);
        break;
      case op_code::push:
        op::push(vm, inst.push);
        break;
      case op_code::pop:
        op::pop(vm, inst.pop);
        break;
      case op_code::jump:
        op::jump(vm, inst.jump);
        break;
      case op_code::move:
        op::move(vm, inst.move);
        break;
      case op_code::compare_i64:
        op::compare_i64(vm, inst.compare);
        break;
      case op_code::compare_f64:
        op::compare_f64(vm, inst.compare);
        break;
      case op_code::conditional_jump:
        op::conditional_jump(vm, inst.conditional_jump);
        break;
      case op_code::conditional_move:
        op::conditional_move(vm, inst.conditional_move);
        break;
      }

      return inst.code != op_code::exit;
    }

    bool step(machine *vm) {
      return decode(vm)
        && execute(vm);
    }
  }
}
