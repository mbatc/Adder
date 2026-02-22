#include "vm.h"
#include "common.h"
#include "compiler.h"
#include "program.h"
#include "compiler/program_builder.h"

#include <iostream>
#include <iomanip>

// #define AD_PRINT_VM_STATE

#define use_small_memcpy 1

#if use_small_memcpy
  #define small_memcpy(dst, src, size) ::adder::impl::tiny_copy(dst, src, size)
#else
  #define small_memcpy(dst, src, size) memcpy(dst, src, size)
#endif

namespace adder {
  namespace impl {
    /// Copy 8 bytes or less
    void tiny_copy(void* dst, void const * src, uint8_t sz) {
      uint8_t* dstData = (uint8_t*)dst;
      uint8_t const * srcData = (uint8_t const * )src;
      switch (sz) {
      case 8:
        *(uint64_t*)dst = *(uint64_t const *)src;
        break;
      case 7:
        dstData[6] = srcData[6];
      case 6:
        dstData[5] = srcData[5];
      case 5:
        dstData[4] = srcData[4];
      case 4:
        *(uint32_t*)dst = *(uint32_t const *)src;
        break;
      case 3:
        dstData[2] = srcData[2];
      case 2:
        *(uint16_t*)dst = *(uint16_t const *)src;
        break;
      case 1:
        dstData[0] = srcData[0];
      case 0:
        break;
      }
    }
  }

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

    void relocate_program(program_view const & program) {
      program_header header = program.get_header();
      uint64_t base = (uint64_t)program.data();

      auto publicSymbols = program.get_public_symbols();
      // auto externSymbols = program.get_extern_symbols();

      for (size_t i = 0; i < header.public_symbol_count; ++i) {
        publicSymbols[i].data_address += base;
        publicSymbols[i].name_address += base;
      }

      uint8_t * pc  = program.data() + program.get_header().code_offset;
      uint8_t * end = pc + program.get_header().code_size;

      while (pc < end) {
        vm::instruction *inst = (vm::instruction*)pc;
        switch (inst->code)
        {
        case vm::op_code::load_addr:
          inst->load_addr.addr += base;
          break;
        case vm::op_code::jump:
        case vm::op_code::conditional_jump:
          inst->jump.addr += base;
          break;
        case vm::op_code::call:
          inst->call.addr += base;
          break;
        }

        pc += instruction_size(inst->code);
      }
    }

    const_program_view load_program(vm::machine* vm, program_view const& program, bool relocated) {
      // Reserve space for the program
      program_view loaded = { (uint8_t*)vm->heap_allocator->allocate(program.size()), program.size() };
      // Write to memory
      memcpy(loaded.data(), program.data(), program.size());
      // Relocate the program to the loaded base address
      if (relocated) {
        relocate_program(loaded);
      }
      return { loaded.data(), loaded.size() };
    }

    namespace op {
      void load(machine * vm, op_code_args<op_code::load> const & args) {
        const void * addr = vm->registers[args.src_addr].ptr;
        const uint8_t *mem  = (const uint8_t*)addr;
        vm->registers[args.dst].u64 = 0;
        memcpy(&vm->registers[args.dst], mem, args.size);
      }

      void load_offset(machine * vm, op_code_args<op_code::load_offset> const & args) {
        const void * addr = vm->registers[args.src_addr].ptr;
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

      void sub_i64(machine * vm, op_code_binary_op_args const & args) {
        const int64_t lhs = vm->registers[args.lhs].i64;
        const int64_t rhs = vm->registers[args.rhs].i64;
        vm->registers[args.dst].i64 = lhs - rhs;
      }

      void sub_f64(machine * vm, op_code_binary_op_args const & args) {
        const double lhs = vm->registers[args.lhs].d64;
        const double rhs = vm->registers[args.rhs].d64;
        vm->registers[args.dst].d64 = lhs - rhs;
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
        small_memcpy(&vm->registers[args.dst], vm->stack.end() - args.size, args.size);

        vm->stack.free(args.size);
        vm->registers[register_names::sp].ptr = vm->stack.end();
      }

      void jump(machine * vm, op_code_args<op_code::jump> const & args) {
        vm->registers[register_names::pc].u64 = args.addr;
      }

      void jump_indirect(machine * vm, op_code_args<op_code::jump_indirect> const & args) {
        vm->registers[register_names::pc] = vm->registers[args.addr];
      }

      void jump_relative(machine * vm, op_code_args<op_code::jump_relative> const & args) {
        vm->registers[register_names::pc].u64 += args.offset - sizeof(vm::instruction);
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
        vm->registers[vm::register_names::rp] = vm->registers[vm::register_names::pc];
        vm->registers[vm::register_names::pc].value = args.addr;
      }

      void call_indirect(machine * vm, op_code_args<op_code::call_indirect> const & args) {
        vm->registers[vm::register_names::rp] = vm->registers[vm::register_names::pc];
        vm->registers[vm::register_names::pc].value = vm->registers[args.addr].value;
      }

      void ret(machine * vm, op_code_args<op_code::ret> const &) {
        vm->registers[vm::register_names::pc] = vm->registers[vm::register_names::rp];
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
      case op_code::sub_i64:
        op::sub_i64(vm, inst.add);
        break;
      case op_code::sub_f64:
        op::sub_f64(vm, inst.add);
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
      case op_code::jump_relative:
        op::jump_relative(vm, inst.jump_relative);
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

    void* compile_call_handle(machine * vm, program_symbol_table_entry const & symbol) {
      compiler::program_builder stub;
      stub.functions.emplace_back();
      stub.function_stack.push_back(0);

      stub.push_return_pointer();
      stub.push_frame_pointer();
      stub.move(register_names::fp, register_names::sp);
      stub.begin_scope();
      stub.call(symbol.data_address);
      stub.end_scope();
      stub.pop_frame_pointer();
      stub.pop_return_pointer();
      
      adder::vm::instruction op;
      op.code = adder::vm::op_code::exit;
      stub.add_instruction(op);
      auto &entry_code = stub.functions[0].instructions;

      void * ret = vm->heap_allocator->allocate(entry_code.size() * sizeof(adder::vm::instruction));
      memcpy(ret, (uint8_t*)entry_code.data(), entry_code.size() * sizeof(adder::vm::instruction));
      return ret;
    }

    void call(machine* vm, void* handle)
    {
      // Set program counter to the entry point.
      vm->registers[adder::vm::register_names::pc].ptr = handle;

#ifdef AD_PRINT_VM_STATE
      int64_t step = 0;
      do
      {
        int i = 0;
        for (auto& reg : vm->registers)
          std::cout << register_to_string(i++) << ": [" << reg.u64 << ", " << reg.i64 << ", " << reg.d64 << "]" << std::endl;

        std::cout << "\nStack:\n";
        for (int p = 0; p < vm->stack.size; ++p)
        {
          if (p % 8 == 0)
          {
            std::cout << std::endl;
            printf("[%lld]: ", (int64_t)(vm->stack.base + p));
          }
          printf("0x%.2x ", vm->stack.base[p]);
        }
        std::cout << "\n\n";

        adder::vm::decode(vm);

        std::cout << "\nInstruction " << step << ": " << op_code_to_string(vm->next_instruction.code) << "\n";

        if (!adder::vm::execute(vm))
        {
          if (vm->next_instruction.code == adder::vm::op_code::exit)
            std::cout << "Finished call\n";
          else
            std::cout << "Failed\n";
          break;
        }

        ++step;
      } while (true);
#else
      while (true)
      {
        bool ok = adder::vm::decode(vm);
        ok = ok && adder::vm::execute(vm);
        if (!ok || vm->next_instruction.code == adder::vm::op_code::exit)
          break;
      }
#endif
    }
  }
}
