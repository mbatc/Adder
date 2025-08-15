#include "vm.h"
#include "common.h"
#include "compiler.h"
#include "compiler/program_builder.h"

namespace adder {
  namespace vm {
    size_t instruction_size(op_code /*code*/) {
      return sizeof(instruction);
    }

    void * allocator::allocate(size_t size) {
      return std::malloc(size);
    }

    void allocator::free(void * ptr) {
      std::free(ptr);
    }

    bool decode(machine * vm) {
      instruction const * pInstruction = reinterpret_cast<instruction const *>((uint8_t*)vm->program_counter());
      size_t sz = instruction_size(pInstruction->code);
      vm->registers[register_names::pc].u64 += sz;
      memcpy(&vm->next_instruction, pInstruction, sz);
      return true;
    }

    void relocate_program(void * program, size_t size) {
      uint8_t * pc  = (uint8_t *)program;
      uint8_t * end = (uint8_t *)program + size;

      while (pc < end) {
        vm::instruction *inst = (vm::instruction*)pc;
        switch (inst->code)
        {
        case vm::op_code::load_addr:
          inst->load_addr.addr += (uint64_t)program;
          break;
        case vm::op_code::jump:
        case vm::op_code::conditional_jump:
          inst->jump.addr += (uint64_t)program;
          break;
        }

        pc += instruction_size(inst->code);
      }
    }

    void * load_program(vm::machine* vm, std::vector<uint8_t> const& program, bool relocated) {
      // Reserve space for the program
      void * baseOffset = vm->heap_allocator->allocate(program.size());
      // Write to memory
      memcpy(baseOffset, program.data(), program.size());

      // Relocate the program to the loaded base address
      if (relocated) {
        relocate_program((uint8_t*)baseOffset, program.size());
      }

      return baseOffset;
    }

    namespace op {
      void load(machine * vm, op_code_args<op_code::load> const & args) {
        const void * addr = vm->registers[args.src_addr].ptr;
        const uint8_t *mem  = (const uint8_t*)addr;
        vm->registers[args.dst].u64 = 0;
        memcpy(&vm->registers[args.dst], mem, args.size);
      }

      void load_offset(machine * vm, op_code_args<op_code::load_offset> const & args) {
        const void * addr = vm->registers[args.addr].ptr;
        const uint8_t *mem  = (const uint8_t*)addr + args.offset;
        vm->registers[args.dst].u64 = 0;
        memcpy(&vm->registers[args.dst], mem, args.size);
      }

      void load_addr(machine * vm, op_code_args<op_code::load_addr> const & args) {
        const uint8_t *mem  = (const uint8_t *)args.addr;
        vm->registers[args.dst].u64 = 0;
        memcpy(&vm->registers[args.dst], mem, args.size);
      }

      void store(machine * vm, op_code_args<op_code::store> const & args) {
        uint8_t *mem = (uint8_t *)vm->registers[args.addr].ptr;
        memcpy(mem, &vm->registers[args.src], args.size);
      }

      void store_offset(machine * vm, op_code_args<op_code::store_offset> const & args) {
        uint8_t *mem = (uint8_t *)vm->registers[args.addr].ptr + args.offset;
        memcpy(mem, &vm->registers[args.src], args.size);
      }

      void store_addr(machine * vm, op_code_args<op_code::store_addr> const & args) {
        uint8_t *mem = (uint8_t *)args.addr;
        memcpy(mem, &vm->registers[args.src], args.size);
      }

      void set(machine * vm, op_code_args<op_code::set> const & args) {
        vm->registers[args.dst].value = args.val;
      }

      void add_i64(machine * vm, op_code_binary_op_args const & args) {
        const int64_t lhs = vm->registers[args.lhs].i64;
        const int64_t rhs = vm->registers[args.rhs].i64;
        vm->registers[args.dst].i64 = lhs + rhs;
      }

      void add_f64(machine * vm, op_code_binary_op_args const & args) {
        const double lhs = vm->registers[args.lhs].d64;
        const double rhs = vm->registers[args.rhs].d64;
        vm->registers[args.dst].d64 = lhs + rhs;
      }

      void mul_i64(machine * vm, op_code_binary_op_args const & args) {
        const int64_t lhs = vm->registers[args.lhs].i64;
        const int64_t rhs = vm->registers[args.rhs].i64;
        vm->registers[args.dst].i64 = lhs * rhs;
      }

      void mul_f64(machine * vm, op_code_binary_op_args const & args) {
        const double lhs = vm->registers[args.lhs].d64;
        const double rhs = vm->registers[args.rhs].d64;
        vm->registers[args.dst].d64 = lhs * rhs;
      }

      void div_i64(machine * vm, op_code_binary_op_args const & args) {
        const int64_t lhs = vm->registers[args.lhs].i64;
        const int64_t rhs = vm->registers[args.rhs].i64;
        vm->registers[args.dst].i64 = lhs / rhs;
      }

      void div_f64(machine * vm, op_code_binary_op_args const & args) {
        const double lhs = vm->registers[args.lhs].d64;
        const double rhs = vm->registers[args.rhs].d64;
        vm->registers[args.dst].d64 = lhs / rhs;
      }

      void alloc_stack(machine * vm, op_code_args<op_code::alloc_stack> const & args) {
        vm->stack.allocate(args.bytes);
        vm->registers[register_names::sp].ptr = vm->stack.end();
      }

      void free_stack(machine * vm, op_code_args<op_code::free_stack> const & args) {
        vm->stack.free(args.bytes);
        vm->registers[register_names::sp].ptr = vm->stack.end();
      }

      void push(machine * vm, op_code_args<op_code::push> const & args) {
        vm->stack.push(&vm->registers[args.src], args.size);
        vm->registers[register_names::sp].ptr = vm->stack.end();
      }

      void pop(machine * vm, op_code_args<op_code::pop> const & args) {
        vm->registers[args.dst].u64 = 0;
        memcpy(&vm->registers[args.dst], vm->stack.end() - args.size, args.size);

        vm->stack.free(args.size);
        vm->registers[register_names::sp].ptr = vm->stack.end();
      }

      void jump(machine * vm, op_code_args<op_code::jump> const & args) {
        vm->registers[register_names::pc].u64 = args.addr;
      }

      void jump_indirect(machine * vm, op_code_args<op_code::jump_indirect> const & args) {
        vm->registers[register_names::pc] = vm->registers[args.addr];
      }

      void move(machine * vm, op_code_args<op_code::move> const & args) {
        vm->registers[args.dst] = vm->registers[args.src];
      }

      void compare_i64(machine * vm, op_code_binary_op_args const & args) {
        const int64_t lhs = vm->registers[args.lhs].i64;
        const int64_t rhs = vm->registers[args.rhs].i64;
        uint64_t &dst = vm->registers[args.dst].u64;
        dst = 0;
        if (lhs < rhs) dst  |= cmp_lt_bit;
        if (lhs == rhs) dst |= cmp_eq_bit;
        if (lhs > rhs) dst  |= cmp_gt_bit;
      }

      void compare_f64(machine * vm, op_code_binary_op_args const & args) {
        const double lhs = vm->registers[args.lhs].d64;
        const double rhs = vm->registers[args.rhs].d64;

        uint64_t &dst = vm->registers[args.dst].u64;
        dst = 0;
        if (lhs < rhs) dst  |= cmp_lt_bit;
        if (lhs == rhs) dst |= cmp_eq_bit;
        if (lhs > rhs) dst  |= cmp_gt_bit;
      }

      void conditional_jump(machine * vm, op_code_args<op_code::conditional_jump> const & args) {
        if (args.cmp_val == (vm->registers[args.cmp_reg].u64 & 0xFF))
          jump(vm, args);
      }

      void conditional_move(machine * vm, op_code_args<op_code::conditional_move> const & args) {
        if (args.cmp_val == (vm->registers[args.cmp_reg].u64 & 0xFF))
          move(vm, args);
      }

      void call(machine * vm, op_code_args<op_code::call> const & args) {
        vm->stack.push(&vm->registers[vm::register_names::fp], sizeof(vm::register_value));
        vm->stack.push(&vm->registers[vm::register_names::rp], sizeof(vm::register_value));
        vm->registers[vm::register_names::sp].ptr = vm->stack.end();

        vm->registers[vm::register_names::rp] = vm->registers[vm::register_names::pc];
        vm->registers[vm::register_names::fp] = vm->registers[vm::register_names::sp];
        vm->registers[vm::register_names::pc].value = args.addr;
      }

      void call_indirect(machine * vm, op_code_args<op_code::call_indirect> const & args) {
        vm->stack.push(&vm->registers[vm::register_names::fp], sizeof(vm::register_value));
        vm->stack.push(&vm->registers[vm::register_names::rp], sizeof(vm::register_value));
        vm->registers[vm::register_names::sp].ptr = vm->stack.end();

        vm->registers[vm::register_names::rp] = vm->registers[vm::register_names::pc];
        vm->registers[vm::register_names::fp] = vm->registers[vm::register_names::sp];
        vm->registers[vm::register_names::pc].value = vm->registers[args.addr].value;
      }

      void ret(machine * vm, op_code_args<op_code::ret> const & args) {
        vm->registers[vm::register_names::pc] = vm->registers[vm::register_names::rp];
        vm->stack.free(vm->stack.end() - (uint8_t*)vm->registers[vm::register_names::fp].ptr);
        vm->stack.pop(&vm->registers[vm::register_names::rp], sizeof(vm::register_value));
        vm->stack.pop(&vm->registers[vm::register_names::fp], sizeof(vm::register_value));
        vm->registers[vm::register_names::sp].ptr = vm->stack.end();
      }
    }

    bool execute(machine * vm) {
      instruction const & inst = vm->next_instruction;
      switch (inst.code) {
      case op_code::load:
        op::load(vm, inst.load);
        break;
      case op_code::load_offset:
        op::load_offset(vm, inst.load_offset);
        break;
      case op_code::load_addr:
        op::load_addr(vm, inst.load_addr);
        break;
      case op_code::store:
        op::store(vm, inst.store);
        break;
      case op_code::store_offset:
        op::store_offset(vm, inst.store_offset);
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
      case op_code::alloc_stack:
        op::alloc_stack(vm, inst.alloc_stack);
        break;
      case op_code::free_stack:
        op::free_stack(vm, inst.free_stack);
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
      case op_code::jump_indirect:
        op::jump_indirect(vm, inst.jump_indirect);
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
      case op_code::call:
        op::call(vm, inst.call);
        break;
      case op_code::call_indirect:
        op::call_indirect(vm, inst.call_indirect);
        break;
      case op_code::ret:
        op::ret(vm, inst.ret);
        break;
      default:
        return false;
      }

      return inst.code != op_code::exit;
    }

    bool step(machine *vm) {
      return decode(vm)
        && execute(vm);
    }

    void* compile_call_handle(machine * vm, void* entryAddr) {
      compiler::program_builder stub;
      stub.call(adder::compiler::program_address{ (uint64_t)entryAddr });
      adder::vm::instruction op;
      op.code = adder::vm::op_code::exit;
      stub.add_instruction(op);

      adder::compiler::program entry_program = stub.binary();
      void * ret = vm->heap_allocator->allocate(stub.code.size() * sizeof(adder::vm::instruction));
      memcpy(ret, (uint8_t*)stub.code.data(), stub.code.size() * sizeof(adder::vm::instruction));
      return ret;
    }
  }
}
