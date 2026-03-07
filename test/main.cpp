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

  void sum_sequence_cb(adder::vm::native_call_context *ctx) {
    int64_t end   = *(int64_t*)adder::vm::native_read_arg(ctx, sizeof(int64_t));
    int64_t start = *(int64_t*)adder::vm::native_read_arg(ctx, sizeof(int64_t));
    int64_t * ret = *(int64_t**)adder::vm::native_read_arg(ctx, sizeof(int64_t*));
    *ret = sum_sequence(start, end);
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

  std::map<std::string, test_details> tests;
  {
    for (auto& item : std::filesystem::directory_iterator(testsRoot)) {
      test_details test;
      if (item.path().extension() != ".ad" || !item.is_regular_file()) {
        continue;
      }

      test.source = read_file(item.path().string());

      std::filesystem::path expectedPath = item.path();
      expectedPath.replace_extension("expected");
      if (std::filesystem::exists(expectedPath)) {
        test.expected_result = adder::test::expected::parse(read_file(expectedPath.string()));
        if (!test.expected_result.has_value()) {
          printf("! Failed to parse expected results: %s", expectedPath.string().c_str());
        }
      }
      else {
        printf("! No expected results defined for %s\n", item.path().string().c_str());
      }

      tests[item.path().string()] = test;
    }
  }

  std::optional<std::string> singleFileTest;
  // singleFileTest = "branch-else-if-chain.ad";
  // singleFileTest = "branch-else.ad";
  // singleFileTest = "branch-if.ad";
  // singleFileTest = "branch-if-false.ad";
  // singleFileTest = "call-recursive.ad";
  // singleFileTest = "function-ptr.ad";
  singleFileTest = "call-native.ad";

  if (singleFileTest.has_value()) {
    tests = {
      { testsRoot + "/" + singleFileTest.value(), {
        read_file(testsRoot + "/" + singleFileTest.value()),
        adder::test::expected{}
      }}
    };
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
      printf("extern_lookup: %s\n", symbol);
      return (adder::vm::address_t)native_methods::sum_sequence_cb;
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
