#include "vm.h"
#include "common.h"
#include "compiler.h"
#include "program.h"
#include "compiler/program_builder.h"

#include <iostream>
#include <iomanip>

// #define AD_PRINT_VM_STATE

#define use_small_memcpy 1
#define use_inline_small_memcpy 1

#define inline_small_memcpy(dst, src, size) do {            \
      uint8_t* dstData = (uint8_t*)dst;                     \
      uint8_t const * srcData = (uint8_t const * )src;      \
      switch (size) {                                       \
      case 8:                                               \
        *(uint64_t*)dstData = *(uint64_t const *)srcData;   \
        break;                                              \
      case 7:                                               \
        dstData[6] = srcData[6];                            \
      case 6:                                               \
        dstData[5] = srcData[5];                            \
      case 5:                                               \
        dstData[4] = srcData[4];                            \
      case 4:                                               \
        *(uint32_t*)dstData = *(uint32_t const *)srcData;   \
        break;                                              \
      case 3:                                               \
        dstData[2] = srcData[2];                            \
      case 2:                                               \
        *(uint16_t*)dstData = *(uint16_t const *)srcData;   \
        break;                                              \
      case 1:                                               \
        dstData[0] = srcData[0];                            \
      case 0:                                               \
        break;                                              \
      }                                                     \
    } while(false)

#if use_small_memcpy
#if use_inline_small_memcpy
  #define small_memcpy(dst, src, size) inline_small_memcpy(dst, src, size)
#else
  #define small_memcpy(dst, src, size) ::adder::impl::tiny_copy(dst, src, size)
#endif
#else
  #define small_memcpy(dst, src, size) memcpy(dst, src, size)
#endif

#define AD_VM_DECODE(vm, pInstruction) \
  pInstruction = (instruction const *)vm->registers[register_names::pc].ptr; \
  vm->registers[register_names::pc].u64 += sizeof(instruction);              \

#define AD_VM_EXECUTE(vm, pInstruction) \
  ::adder::vm::instruction_table[(uint8_t)pInstruction->code](vm, pInstruction); \

#define AD_VM_GROW_STACK(vm)                                                       \
 do {                                                                              \
    const size_t curCap = vm->stack.end - vm->stack.base;                          \
    const size_t newCap = curCap * 2;                                              \
    void * newData      = vm->heap_allocator->allocate(newCap);                    \
    const size_t curSz  = vm->registers[register_names::sp].data - vm->stack.base; \
    memcpy(newData, vm->stack.base, curSz);                                        \
    vm->heap_allocator->free(vm->stack.base);                                      \
    vm->stack.base = (uint8_t*)newData;                                            \
    vm->registers[register_names::sp].data = vm->stack.base + curSz;               \
  } while(false)

namespace adder {
  namespace impl {
    /// Copy 8 bytes or less
    void tiny_copy(void* dst, void const * src, uint8_t sz) {
      inline_small_memcpy(dst, src, sz);
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

    void relocate_program(program_view const & program) {
      program_header header = program.get_header();
      uint64_t base = (uint64_t)program.data();

      auto publicSymbols = program.get_public_symbols();
      // auto externSymbols = program.get_extern_symbols();

      for (size_t i = 0; i < header.public_symbol_count; ++i) {
        publicSymbols[i].data_address += base;
        publicSymbols[i].name_address += base;
      }

      for (program_relocation_table_entry * relocation = program.first_relocation_entry(); relocation != nullptr; relocation = program.next_relocation_entry(relocation)) {
        auto addr = publicSymbols[relocation->symbol].data_address;
        uint64_t const * offsets = (uint64_t const *)(relocation + 1);
        for (size_t i = 0; i < relocation->count; ++i) {
          *(uint64_t*)(base + offsets[i]) += addr;
        }
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

      void * initializer = adder::vm::compile_call_handle(vm, *loaded.find_public_symbol("()=>void:$module_init"));
      adder::vm::call(vm, initializer);
      adder::vm::free(vm, initializer);

      return { loaded.data(), loaded.size() };
    }

    namespace op {
      inline void load(machine * vm, op_code_args<op_code::load> const & args) {
        const void * addr = vm->registers[args.src_addr].ptr;
        const uint8_t *mem  = (const uint8_t*)addr;
        vm->registers[args.dst].u64 = 0;
        small_memcpy(&vm->registers[args.dst], mem, args.size);
      }

      inline void load_offset(machine * vm, op_code_args<op_code::load_offset> const & args) {
        const void    * addr = vm->registers[args.src_addr].ptr;
        const uint8_t * mem  = (const uint8_t*)addr + args.offset;
        vm->registers[args.dst].u64 = 0;
        small_memcpy(&vm->registers[args.dst], mem, args.size);
      }

      inline void load_addr(machine * vm, op_code_args<op_code::load_addr> const & args) {
        const uint8_t *mem  = (const uint8_t *)args.addr;
        vm->registers[args.dst].u64 = 0;
        small_memcpy(&vm->registers[args.dst], mem, args.size);
      }

      inline void store(machine * vm, op_code_args<op_code::store> const & args) {
        uint8_t *mem = (uint8_t *)vm->registers[args.addr].ptr;
        small_memcpy(mem, &vm->registers[args.src], args.size);
      }

      inline void store_offset(machine * vm, op_code_args<op_code::store_offset> const & args) {
        uint8_t *mem = (uint8_t *)vm->registers[args.addr].ptr + args.offset;
        small_memcpy(mem, &vm->registers[args.src], args.size);
      }

      inline void store_addr(machine * vm, op_code_args<op_code::store_addr> const & args) {
        uint8_t *mem = (uint8_t *)args.addr;
        small_memcpy(mem, &vm->registers[args.src], args.size);
      }

      inline void store_value(machine * vm, op_code_args<op_code::store_value> const & args) {
        uint8_t *mem = (uint8_t *)vm->registers[args.addr].ptr;
        small_memcpy(mem, &args.src, args.size);
      }

      inline void store_value_offset(machine * vm, op_code_args<op_code::store_value_offset> const & args) {
        uint8_t *mem = (uint8_t *)vm->registers[args.addr].ptr + args.offset;
        small_memcpy(mem, &args.src, args.size);
      }

      inline void store_value_addr(machine * vm, op_code_args<op_code::store_value_addr> const & args) {
        unused(vm);
        uint8_t *mem = (uint8_t *)args.addr;
        small_memcpy(mem, &args.src, args.size);
      }

      inline void set(machine * vm, op_code_args<op_code::set> const & args) {
        vm->registers[args.dst].value = args.val;
      }

      inline void add_i64(machine * vm, op_code_binary_op_args const & args) {
        const int64_t lhs = vm->registers[args.lhs].i64;
        const int64_t rhs = vm->registers[args.rhs].i64;
        vm->registers[args.dst].i64 = lhs + rhs;
      }

      inline void add_i64_constant(machine * vm, op_code_binary_op_args_reg_constant const & args) {
        const int64_t lhs = vm->registers[args.lhs].i64;
        const int64_t rhs = static_cast<int64_t>(args.rhs);
        vm->registers[args.dst].i64 = lhs + rhs;
      }

      inline void add_f64(machine * vm, op_code_binary_op_args const & args) {
        const double lhs = vm->registers[args.lhs].d64;
        const double rhs = vm->registers[args.rhs].d64;
        vm->registers[args.dst].d64 = lhs + rhs;
      }

      inline void sub_i64(machine * vm, op_code_binary_op_args const & args) {
        const int64_t lhs = vm->registers[args.lhs].i64;
        const int64_t rhs = vm->registers[args.rhs].i64;
        vm->registers[args.dst].i64 = lhs - rhs;
      }

      inline void sub_f64(machine * vm, op_code_binary_op_args const & args) {
        const double lhs = vm->registers[args.lhs].d64;
        const double rhs = vm->registers[args.rhs].d64;
        vm->registers[args.dst].d64 = lhs - rhs;
      }

      inline void mul_i64(machine * vm, op_code_binary_op_args const & args) {
        const int64_t lhs = vm->registers[args.lhs].i64;
        const int64_t rhs = vm->registers[args.rhs].i64;
        vm->registers[args.dst].i64 = lhs * rhs;
      }

      inline void mul_f64(machine * vm, op_code_binary_op_args const & args) {
        const double lhs = vm->registers[args.lhs].d64;
        const double rhs = vm->registers[args.rhs].d64;
        vm->registers[args.dst].d64 = lhs * rhs;
      }

      inline void div_i64(machine * vm, op_code_binary_op_args const & args) {
        const int64_t lhs = vm->registers[args.lhs].i64;
        const int64_t rhs = vm->registers[args.rhs].i64;
        vm->registers[args.dst].i64 = lhs / rhs;
      }

      inline void div_f64(machine * vm, op_code_binary_op_args const & args) {
        const double lhs = vm->registers[args.lhs].d64;
        const double rhs = vm->registers[args.rhs].d64;
        vm->registers[args.dst].d64 = lhs / rhs;
      }

      inline void alloc_stack(machine * vm, op_code_args<op_code::alloc_stack> const & args) {
        if (vm->registers[register_names::sp].data + args.bytes >= vm->stack.end) {
          AD_VM_GROW_STACK(vm);
        }
        vm->registers[register_names::sp].data += args.bytes;
      }

      inline void free_stack(machine * vm, op_code_args<op_code::free_stack> const & args) {
        vm->registers[register_names::sp].data -= args.bytes;
      }

      inline void push(machine * vm, op_code_args<op_code::push> const & args) {
        if (vm->registers[register_names::sp].data + args.size >= vm->stack.end) {
          AD_VM_GROW_STACK(vm);
        }

        small_memcpy(vm->registers[register_names::sp].data, &vm->registers[args.src], args.size);
        vm->registers[register_names::sp].data += args.size;
      }

      inline void pop(machine * vm, op_code_args<op_code::pop> const & args) {
        vm->registers[args.dst].u64 = 0;
        vm->registers[register_names::sp].data -= args.size;
        small_memcpy(&vm->registers[args.dst], vm->registers[register_names::sp].data, args.size);
      }

      inline void jump(machine * vm, op_code_args<op_code::jump> const & args) {
        vm->registers[register_names::pc].u64 = args.addr;
      }

      inline void jump_indirect(machine * vm, op_code_args<op_code::jump_indirect> const & args) {
        vm->registers[register_names::pc] = vm->registers[args.addr];
      }

      inline void jump_relative(machine * vm, op_code_args<op_code::jump_relative> const & args) {
        vm->registers[register_names::pc].u64 += args.offset - sizeof(vm::instruction);
      }

      inline void move(machine * vm, op_code_args<op_code::move> const & args) {
        vm->registers[args.dst] = vm->registers[args.src];
      }

      inline void bitwise_and(machine * vm, op_code_bitwise_op_args const & args) {
        vm->registers[args.lhs].value &= vm->registers[args.rhs].value;
      }

      inline void bitwise_or(machine * vm, op_code_bitwise_op_args const & args) {
        vm->registers[args.lhs].value |= vm->registers[args.rhs].value;
      }

      inline void bitwise_xor(machine * vm, op_code_bitwise_op_args const & args) {
        vm->registers[args.lhs].value ^= vm->registers[args.rhs].value;
      }

      inline void bitwise_and_value(machine * vm, op_code_bitwise_op_constant_args const & args) {
        vm->registers[args.reg].value &= args.val;
      }

      inline void bitwise_or_value(machine * vm, op_code_bitwise_op_constant_args const & args) {
        vm->registers[args.reg].value |= args.val;
      }

      inline void bitwise_xor_value(machine * vm, op_code_bitwise_op_constant_args const & args) {
        vm->registers[args.reg].value ^= args.val;
      }

      inline void set_non_zero(machine* vm, op_code_args<op_code::set_non_zero> const & args) {
        vm->registers[args.dst].i64 = (int64_t)(vm->registers[args.dst].i64 == 0 ? args.if_zero : args.if_non_zero);
      }

      inline void compare_i64(machine * vm, op_code_binary_op_args const & args) {
        const int64_t lhs = vm->registers[args.lhs].i64;
        const int64_t rhs = vm->registers[args.rhs].i64;
        uint64_t &dst = vm->registers[args.dst].u64;
        dst = 0;
        if (lhs < rhs) dst  |= cmp_lt_bit;
        if (lhs == rhs) dst |= cmp_eq_bit;
        if (lhs > rhs) dst  |= cmp_gt_bit;
      }

      inline void compare_f64(machine * vm, op_code_binary_op_args const & args) {
        const double lhs = vm->registers[args.lhs].d64;
        const double rhs = vm->registers[args.rhs].d64;

        uint64_t &dst = vm->registers[args.dst].u64;
        dst = 0;
        if (lhs < rhs) dst  |= cmp_lt_bit;
        if (lhs == rhs) dst |= cmp_eq_bit;
        if (lhs > rhs) dst  |= cmp_gt_bit;
      }

      inline void conditional_jump(machine * vm, op_code_args<op_code::conditional_jump> const & args) {
        if (args.cmp_val == (vm->registers[args.cmp_reg].u64 & 0xFF))
          jump(vm, args);
      }

      inline void conditional_move(machine * vm, op_code_args<op_code::conditional_move> const & args) {
        if (args.cmp_val == (vm->registers[args.cmp_reg].u64 & 0xFF))
          move(vm, args);
      }

      inline void call(machine * vm, op_code_args<op_code::call> const & args) {
        vm->registers[vm::register_names::rp] = vm->registers[vm::register_names::pc];
        vm->registers[vm::register_names::pc].value = args.addr;
      }

      inline void call_indirect(machine * vm, op_code_args<op_code::call_indirect> const & args) {
        vm->registers[vm::register_names::rp] = vm->registers[vm::register_names::pc];
        vm->registers[vm::register_names::pc].value = vm->registers[args.addr].value;
      }

      inline void ret(machine * vm, op_code_args<op_code::ret> const &) {
        vm->registers[vm::register_names::pc] = vm->registers[vm::register_names::rp];
      }
    }

    using InstructionMethod = void (*)(machine*, instruction const *);
    static InstructionMethod instruction_table[(uint8_t)op_code::count] = {
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::exit); vm; inst; },                                                              // exit
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::noop); vm; inst; },                                                              // noop
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::load); op::load(vm, inst->load); },                                  // load
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::load_addr); op::load_addr(vm, inst->load_addr); },                            // load_addr
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::load_offset );op::load_offset(vm, inst->load_offset); },                            // load_offset
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::store); op::store(vm, inst->store); },                                  // store
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::store_addr); op::store_addr(vm, inst->store_addr); },                            // store_addr
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::store_offset); op::store_offset(vm, inst->store_offset); },                            // store_offset
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::store_value); op::store_value(vm, inst->store_value); },                            // store_value
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::store_value_addr); op::store_value_addr(vm, inst->store_value_addr); },               // store_value_addr
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::store_value_offset); op::store_value_offset(vm, inst->store_value_offset); },               // store_value_offset
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::set); op::set(vm, inst->set); },                                     // set
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::add_i64); op::add_i64(vm, inst->add); },                                    // add_i64
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::add_i64_constant); op::add_i64_constant(vm, inst->add_constant); },               // add_i64_constant
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::add_f64); op::add_f64(vm, inst->add); },                               // add_f64
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::sub_i64); op::sub_i64(vm, inst->sub); },                               // sub_i64
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::sub_f64); op::sub_f64(vm, inst->sub); },                               // sub_f64
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::mul_i64); op::mul_i64(vm, inst->mul); },                               // mul_i64
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::mul_f64); op::mul_f64(vm, inst->mul); },                               // mul_f64
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::div_i64); op::div_i64(vm, inst->div); },                               // div_i64
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::div_f64); op::div_f64(vm, inst->div); },                               // div_f64
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::alloc_stack); op::alloc_stack(vm, inst->alloc_stack); },                   // alloc_stack
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::free_stack); op::free_stack(vm, inst->free_stack); },                     // free_stack
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::push); op::push(vm, inst->push); },                                   // push
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::pop); op::pop(vm, inst->pop); },                                   // pop
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::jump); op::jump(vm, inst->jump); },                                   // jump
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::jump_relative); op::jump_relative(vm, inst->jump_relative); },               // jump_relative
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::jump_indirect); op::jump_indirect(vm, inst->jump_indirect); },               // jump_indirect
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::move); op::move(vm, inst->move); },                              // move
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::bitwise_and); op::bitwise_and(vm, inst->bitwise_op); },                                                                                 // bitwise_and
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::bitwise_or); op::bitwise_or(vm, inst->bitwise_op); },                                                                                // bitwise_or
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::bitwise_xor); op::bitwise_xor(vm, inst->bitwise_op); },                                                                                 // bitwise_xor
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::bitwise_and_value); op::bitwise_and_value(vm, inst->bitwise_op_constant); },                                                                                 // bitwise_and_value
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::bitwise_or_value); op::bitwise_or_value(vm, inst->bitwise_op_constant); },                                                                                // bitwise_or_value
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::bitwise_xor_value); op::bitwise_xor_value(vm, inst->bitwise_op_constant); },                                                                                 // bitwise_xor_value
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::set_non_zero); op::set_non_zero(vm, inst->set_non_zero); },                                                                                // set_non_zero
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::compare_i64); op::compare_i64(vm, inst->compare); },                       // compare_i64
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::compare_f64); op::compare_f64(vm, inst->compare); },                       // compare_f64
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::conditional_jump); op::conditional_jump(vm, inst->conditional_jump); },               // conditional_jump
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::conditional_move); op::conditional_move(vm, inst->conditional_move); },               // conditional_move
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::call); op::call(vm, inst->call); },                                // call
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::call_indirect); op::call_indirect(vm, inst->call_indirect); },               // call_indirect
      [](machine * vm, instruction const * inst) { assert(inst->code == op_code::ret); op::ret(vm, inst->ret); },                                 // ret
    };

    void * compile_call_handle(machine * vm, program_symbol_table_entry const & symbol) {
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

    void free(machine * vm, void* ptr) {
      vm->heap_allocator->free(ptr);
    }

    void call(machine* vm, void* handle)
    {
      // Set program counter to the entry point.
      vm->registers[adder::vm::register_names::pc].ptr = handle;

#ifdef AD_PRINT_VM_STATE
      int64_t step = 0;
      instruction const* pInstruction = nullptr;
      do
      {
        int i = 0;
        for (auto& reg : vm->registers)
          std::cout << register_to_string(i++) << ": [" << reg.u64 << ", " << reg.i64 << ", " << reg.d64 << "]" << std::endl;

        std::cout << "\nStack:\n";
        for (uint8_t* p = vm->stack.base; p < vm->registers[vm::register_names::sp].data; ++p)
        {
          if ((int64_t)p % 8 == 0)
          {
            std::cout << std::endl;
            printf("[%lld]: ", (int64_t)p);
          }
          printf("0x%.2x ", *p);
        }
        std::cout << "\n\n";

        AD_VM_DECODE(vm, pInstruction);

        std::cout << "\nInstruction " << step << ": " << op_code_to_string(pInstruction->code) << "\n";

        AD_VM_EXECUTE(vm, pInstruction);
        ++step;
      } while (pInstruction->code != op_code::exit);

      std::cout << "Finished call\n";
#else
      instruction const* pInstruction = nullptr;
      do
      {
        pInstruction = (instruction const *)vm->registers[register_names::pc].ptr;
        vm->registers[register_names::pc].u64 += sizeof(instruction);
        instruction_table[(uint8_t)pInstruction->code](vm, pInstruction);
      } while (pInstruction->code != op_code::exit);
#endif
    }
  }
}
