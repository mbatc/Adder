#include "vm.h"
#include "compiler.h"
#include "compiler/program_builder.h"
#include "common.h"
#include "program.h"
#include "expected.h"

#include <iostream>
#include <chrono>
#include <filesystem>

namespace native_methods {
  int64_t sum_sequence(int64_t start, int64_t end) {
    int64_t sum = 0;
    for (int64_t i = start; i < end; ++i) {
      sum += i;
    }
    return sum;
  }

  void sum_sequence_cb(adder::vm::call_context *ctx) {
    int64_t end   = *(int64_t*)adder::vm::call_context_read_arg(ctx, sizeof(int64_t));  // param 1
    int64_t start = *(int64_t*)adder::vm::call_context_read_arg(ctx, sizeof(int64_t));  // param 0
    int64_t *ret  =  (int64_t*)adder::vm::call_context_read_arg(ctx, sizeof(int64_t*)); // return
    *ret = sum_sequence(start, end);
  }

  void reentrant_cb(adder::vm::call_context *ctx) {
    adder::vm::address_t callback = *(adder::vm::address_t*)adder::vm::call_context_read_arg(ctx, sizeof(int64_t));
    adder::vm::machine * vm = adder::vm::call_context_get_machine(ctx);
    void * handle = adder::vm::compile_call_handle(vm, callback);

    // TODO: When we can call with args and handle return values
    // adder::vm::push_return_value();
    // adder::vm::push_arg();
    adder::vm::call(vm, handle);
    // adder::vm::pop_return_value();
    adder::vm::free(vm, handle);
    handle = nullptr;
  }
}

std::string read_file(std::string const & path) {
  std::string content;
  FILE * file = 0;

#ifdef _WIN32
  if (fopen_s(&file, path.c_str(), "r") != 0)
    return "";
#else
  file = fopen();
#endif
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
  std::string testsRoot = "../../test/cases";
  bool testPerf = false;

  struct test_details {
    std::string source;
    std::optional<adder::test::expected> expected_result;
  };

  const auto add_test_details = [](std::map<std::string, test_details> *tests, std::filesystem::path srcFile) {
    test_details test;
    test.source = read_file(srcFile.string());
    std::filesystem::path expectedPath = srcFile;
    expectedPath.replace_extension("expected");
    if (std::filesystem::exists(expectedPath)) {
      test.expected_result = adder::test::expected::parse(read_file(expectedPath.string()));
      if (!test.expected_result.has_value()) {
        printf("! Failed to parse expected results: %s", expectedPath.string().c_str());
      }
    }
    else {
      printf("! No expected results defined for %s\n", srcFile.string().c_str());
    }

    (*tests)[srcFile.string()] = test;
  };

  std::map<std::string, test_details> tests;
  for (auto& item : std::filesystem::directory_iterator(testsRoot)) {
    if (item.path().extension() != ".ad" || !item.is_regular_file()) {
      continue;
    }

    add_test_details(&tests, item.path());
  }

  std::optional<std::string> singleFileTest;
  // singleFileTest = "branch-else-if-chain.ad";
  // singleFileTest = "branch-else.ad";
  // singleFileTest = "branch-if.ad";
  // singleFileTest = "branch-if-false.ad";
  // singleFileTest = "call-recursive.ad";
  // singleFileTest = "function-ptr.ad";
  // singleFileTest = "call-native.ad";
  // singleFileTest = "call-native-reentrant.ad";

  if (singleFileTest.has_value()) {
    tests.clear();
    add_test_details(&tests, testsRoot + "/" + singleFileTest.value());
  }

  std::vector<std::string> failed;

  for (auto& [file, test] : tests) {
    printf("Compile and run: %s\n", file.c_str());

    auto result = adder::compile(test.source);
    if (!result.has_value()) {
      printf("! Failed to compile: %s\n", file.c_str());
      continue;
    }

    adder::vm::allocator allocator;
    adder::vm::machine vm(&allocator);

    vm.lookup_extern_symbol = [](char const * symbol) -> adder::vm::address_t {
      if (strcmp(symbol, "(int64,int64)=>int64:sum_sequence") == 0)
        return (adder::vm::address_t)native_methods::sum_sequence_cb;
      if (strcmp(symbol, "([ref]()=>void)=>int64:reentrant") == 0)
        return (adder::vm::address_t)native_methods::reentrant_cb;
      return 0;
    };

    bool ok = true;
    if (test.expected_result.has_value()) {
      auto loaded = adder::vm::load_program(&vm, result->view());
      void * entry = nullptr;
      if (test.expected_result->entry.has_value()) {
        auto entrySymbol = loaded.find_public_symbol(test.expected_result->entry.value());
        if (entrySymbol != nullptr) {
          entry = adder::vm::compile_call_handle(&vm, *entrySymbol);
          adder::vm::call(&vm, entry);
          printf("  |   OK   | entry: %s\n", test.expected_result->entry->c_str());
        }
        else {
          printf("  | FAILED | entry: %s, does not exist\n", test.expected_result->entry->c_str());
          ok = false;
        }
      }

      for (auto& symbols : test.expected_result->symbols) {
        auto const testResultSymbol = loaded.find_public_symbol(symbols.name);
        bool const exists           = testResultSymbol != nullptr;
        bool const correctExists    = symbols.exists == exists;

        std::optional<int64_t> expectedValue;
        if (symbols.value.length() > 0)
          expectedValue = std::strtoll(symbols.value.c_str(), 0, 10);

        bool const correctValue = !expectedValue.has_value() || (exists && expectedValue == *(int64_t*)testResultSymbol->data_address);
        if (correctValue && correctExists) {
          printf("  |   OK   | symbol: %s, exists=%s", symbols.name.c_str(), exists ? "true" : "false");
          if (exists && expectedValue.has_value())
            printf(", value=%lld", *(int64_t*)testResultSymbol->data_address);
        }
        else {
          printf("  | FAILED | symbol: %s", symbols.name.c_str());
          if (!correctExists) {
            printf(", exists=%s (expected %s)", exists ? "true" : "false", symbols.exists ? "true" : "false");
          }
          if (!correctValue && testResultSymbol != nullptr) {
            printf(", value=%lld (expected %lld)", *(int64_t*)testResultSymbol->data_address, expectedValue.value());
          }
          ok = false;
        }
        printf("\n");
      }
      if (ok && testPerf && entry) {
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
        printf("  |  PERF  | Average run time of %lld calls: %.2f\n", batchSize, tm);
      }

      if (entry != nullptr)
        adder::vm::free(&vm, entry);

      if (!ok)
        failed.push_back(file);
    }
  }

  return failed.empty() ? 0 : 1;
}
