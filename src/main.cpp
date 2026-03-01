#include "vm.h"
#include "compiler.h"
#include "compiler/program_builder.h"
#include "common.h"
#include "program.h"

#include <iostream>
#include <chrono>
#include <filesystem>

std::string read_file(std::string const & path) {
  std::string content;
  FILE * file = fopen(path.c_str(), "r");
  while (!feof(file)) {
    char buffer[1024];
    size_t numRead = fread(buffer, 1, sizeof(buffer), file);
    content += std::string_view(buffer, numRead);
  }
  fclose(file);
  return content;
}

int main(int argc, char ** argv) {
  adder::unused(argc, argv);
  const bool testPerf = false;
  std::map<std::string, std::string> tests;
  {
    for (auto& item : std::filesystem::directory_iterator( "../../tests/")) {
      if (item.is_regular_file()) {
        tests[item.path().string()] = read_file(item.path().string());
      }
    }
  }

  std::optional<std::string> singleFileTest;
  singleFileTest = "branch-else-if-chain.ad";
  // singleFileTest = "branch-else.ad";
  // singleFileTest = "branch-if.ad";
  // singleFileTest = "branch-if-false.ad";

  if (singleFileTest.has_value()) {
    tests = {
      { "../../tests/" + singleFileTest.value(), read_file("../../tests/" + singleFileTest.value())}
    };
  }

  for (auto& [file, source] : tests) {
    printf("Compile and run: %s\n", file.c_str());

    auto result = adder::compile(source);
    if (!result.has_value()) {
      printf("! Failed to compile: %s\n", file.c_str());
      continue;
    }

    adder::vm::allocator allocator;
    adder::vm::machine vm(&allocator);

    auto loaded = adder::vm::load_program(&vm, result->view());
    auto mainSymbol = loaded.find_public_symbol("()=>void:main");
    void * entry = nullptr;

    if (mainSymbol != nullptr) {
      entry = adder::vm::compile_call_handle(&vm, *mainSymbol);
      adder::vm::call(&vm, entry);
    }
    else {
      printf(" - %s has no ()=>void:main symbol.\n", file.c_str());
    }

    auto testResultSymbol = loaded.find_public_symbol("int64:test_result");
    if (testResultSymbol != nullptr) {
      printf(" - test_result: %lld\n", *(int64_t*)testResultSymbol->data_address);
    }
    else {
      printf(" - %s has no int64:test_result symbol.\n", file.c_str());
    }

    if (testPerf && entry) {
      double tm = 0;
      const int64_t batches = 100;
      const int64_t batchSize = 100000;
      for (int64_t j = 0; j < batches; ++j) {
        using namespace std::chrono;
        auto start = high_resolution_clock::now();

        for (int64_t i = 0; i < batchSize; ++i)
          adder::vm::call(&vm, entry);

        auto end = high_resolution_clock::now();

        tm += (double)(end - start).count() / (1000 * 1000 * 1000ll);
      }
      tm /= 100;
      printf(" - Average run time of %lld calls: %.2f\n", batchSize, tm);
    }

    if (entry != nullptr)
      adder::vm::free(&vm, entry);
  }

  return 0;
}
