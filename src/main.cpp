#include "vm.h"
#include "compiler.h"
#include "compiler/program_builder.h"
#include "common.h"

#include <iostream>
#include <iomanip>

inline static std::string op_code_to_string(adder::vm::op_code op) {
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

int main(int argc, char ** argv) {
  adder::unused(argc, argv);

  std::string content;
  {
    FILE * file = fopen("tests/simple-assign.ad", "r");
    while (!feof(file)) {
      char buffer[1024];
      size_t numRead = fread(buffer, 1, sizeof(buffer), file);
      content += std::string_view(buffer, numRead);
    }
    fclose(file);
  }

  auto result = adder::compile(content);

  adder::vm::machine vm;
  uint64_t base  = adder::vm::load_program(&vm, result.binary);
  uint64_t mainAddr = 0;
  for (uint64_t i = 0; i < result.get_header().public_symbol_count; ++i) {
    auto name = result.get_symbol(
      result.get_public_symbols()[i]
    );
    if (name == "fn:()=>void:main") {
      mainAddr = base + result.get_public_symbols()[i].data_address;
      break;
    }
  }

  adder::compiler::program_builder stub;
  stub.push_return_pointer();
  stub.jump_to(adder::compiler::program_address{ mainAddr });
  adder::compiler::program entry_program = stub.binary();
  uint64_t entry = vm.memory.heap.allocate(stub.code.size() * sizeof(adder::vm::instruction));
  vm.memory.heap.write(entry, (uint8_t*)stub.code.data(), stub.code.size() * sizeof(adder::vm::instruction));

  // uint64_t entry_base = adder::vm::load_program(&vm, entry_program.binary, false);
  // uint64_t entry = entry_base + entry_program.get_header().code_offset;
  // Set program counter to the entry point.
  vm.registers[adder::vm::register_names::pc].u64 = entry;

  int64_t step = 0;
  do
  {
    int i = 0;
    for (auto &reg : vm.registers)
      std::cout << register_to_string(i++) << ": [" << reg.u64 << ", " << reg.i64 << ", " << reg.d64 << "]" << std::endl;

    std::cout << "\nStack:\n";
    for (int p = 0; p < vm.memory.stack_size; ++p)
    {
      if (p % 8 == 0)
        std::cout << std::endl;
      printf("0x%.2x ", vm.memory.stack[p]);
    }
    std::cout << "\n\n";

    adder::vm::decode(&vm);

    std::cout << "\Instruction " << step << ": " << op_code_to_string(vm.next_instruction.code) << "\n";

    if (!adder::vm::execute(&vm))
    {
      printf("Failed\n");
      break;
    }

    ++step;

    if (vm.registers[adder::vm::register_names::pc].u64 == entry)
    {
      printf("Finished call\n");
      break;
    }
  } while (true);

  return 0;
}
