#pragma once

#include <string_view>
#include <string>
#include <vector>

namespace adder {
  // Compiler implementation
  namespace compiler {
    struct program {
      struct header {
        uint32_t version              = 1;
        uint32_t header_size          = sizeof(header);
        uint64_t public_symbol_offset = 0;
        uint64_t public_symbol_count  = 0;
        uint64_t extern_symbol_offset = 0;
        uint64_t extern_symbol_count  = 0;
        uint64_t symbol_data_offset   = 0;
        uint64_t symbol_data_size     = 0;
        uint64_t program_data_offset  = 0;
        uint64_t program_data_size    = 0;
        uint64_t code_offset          = 0;
        uint64_t code_size            = 0;
      };

      struct symbol_table_entry {
        uint64_t name_address = 0;
        uint64_t data_address = 0;
      };

      program() = default;
      program(std::vector<uint8_t> binary) : binary(std::move(binary)) {}

      header & get_header() {
        return *(header*)&binary[0];
      }

      header const & get_header() const {
        return *(header const *)&binary[0];
      }

      symbol_table_entry * get_public_symbols() {
        uint8_t * table = &binary[0] + get_header().public_symbol_offset;
        return (symbol_table_entry*)table;
      }

      symbol_table_entry const * get_public_symbols() const {
        uint8_t const * table = &binary[0] + get_header().public_symbol_offset;
        return (symbol_table_entry const *)table;
      }

      symbol_table_entry * get_extern_symbols() {
        uint8_t * table = &binary[0] + get_header().extern_symbol_offset;
        return (symbol_table_entry*)table;
      }

      symbol_table_entry const * get_extern_symbols() const {
        uint8_t const * table = &binary[0] + get_header().extern_symbol_offset;
        return (symbol_table_entry const *)table;
      }

      std::string_view get_symbol(symbol_table_entry const & entry) const {
        return (char const *)&binary[entry.name_address];
      }

      std::vector<uint8_t> binary;
    };
  }

  compiler::program compile(std::string const & source);
}
