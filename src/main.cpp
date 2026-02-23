#include "vm.h"
#include "compiler.h"
#include "compiler/program_builder.h"
#include "common.h"
#include "program.h"

#include <iostream>
#include <chrono>

int main(int argc, char ** argv) {
  adder::unused(argc, argv);

  std::string content;
  {
    FILE * file = fopen("../../tests/simple-call.ad", "r");
    while (!feof(file)) {
      char buffer[1024];
      size_t numRead = fread(buffer, 1, sizeof(buffer), file);
      content += std::string_view(buffer, numRead);
    }
    fclose(file);
  }

  auto result = adder::compile(content);

  adder::vm::allocator allocator;
  adder::vm::machine vm(&allocator);

  auto loaded = adder::vm::load_program(&vm, result.view());
  auto mainSymbol = loaded.find_public_symbol("()=>void:main");

  void * entry = adder::vm::compile_call_handle(&vm, *mainSymbol);

  adder::vm::call(&vm, entry);

  // for (int64_t j = 0; j < 100; ++j)
  // {
  //   using namespace std::chrono;
  //   auto start = high_resolution_clock::now();
  // 
  //   for (int64_t i = 0; i < 100000; ++i)
  // 
  //   auto end = high_resolution_clock::now();
  // 
  //   double s = (double)(end - start).count() / (1000 * 1000 * 1000ll);
  //   printf("%.4f\n", s);
  // }
  return 0;
}
