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
