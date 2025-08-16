
namespace adder {
  struct program_header {
    uint32_t version = 1;
    uint32_t header_size = sizeof(program_header);
    uint64_t public_symbol_offset = 0;
    uint64_t public_symbol_count = 0;
    uint64_t extern_symbol_offset = 0;
    uint64_t extern_symbol_count = 0;
    uint64_t symbol_data_offset = 0;
    uint64_t symbol_data_size = 0;
    uint64_t program_data_offset = 0;
    uint64_t program_data_size = 0;
    uint64_t code_offset = 0;
    uint64_t code_size = 0;
  };

  struct program_symbol_table_entry {
    uint64_t name_address = 0;
    uint64_t data_address = 0;
  };

  template<bool Const>
  struct program_view_impl {
    using vector_t       = std::conditional_t<Const, const std::vector<uint8_t>, std::vector<uint8_t>>;
    using storage_t      = std::conditional_t<Const, const uint8_t, uint8_t>;
    using header_t       = std::conditional_t<Const, const program_header, program_header>;
    using symbol_entry_t = std::conditional_t<Const, const program_symbol_table_entry, program_symbol_table_entry>;

    program_view_impl() = default;

    program_view_impl(vector_t &data)
      : ptr(data.data())
      , bytes(data.size())
    {}

    program_view_impl(storage_t * ptr, size_t size)
      : ptr(ptr)
      , bytes(size)
    {}

    header_t& get_header() const {
      return *(header_t*)ptr;
    }

    symbol_entry_t * get_public_symbols() const {
      uint8_t const* table = ptr + get_header().public_symbol_offset;
      return (symbol_entry_t*)table;
    }

    symbol_entry_t * find_public_symbol(std::string_view const & find) const {
      for (uint64_t i = 0; i < get_header().public_symbol_count; ++i) {
        auto name = get_symbol(get_public_symbols()[i]);
        if (name == find) {
          return get_public_symbols() + i;
        }
      }
      return nullptr;
    }

    symbol_entry_t * get_extern_symbols() const {
      uint8_t const* table = ptr + get_header().extern_symbol_offset;
      return (symbol_entry_t *)table;
    }

    std::string_view get_symbol(program_symbol_table_entry const& entry) const {
      return (char const*)entry.name_address;
    }

    storage_t * data() const { return ptr; }
    size_t size() const { return bytes; }

    storage_t * ptr = nullptr;
    size_t bytes  = 0;
  };
  using program_view       = program_view_impl<false>;
  using const_program_view = program_view_impl<true>;

  struct program {
    program(std::vector<uint8_t> const& data)
      : data(data)
    {}

    program_header const& get_header() const {
      return view().get_header();
    }

    program_symbol_table_entry const* get_public_symbols() const {
      return view().get_public_symbols();
    }

    program_symbol_table_entry const* find_public_symbol(std::string_view const& name) const {
      return view().find_public_symbol(name);
    }

    program_symbol_table_entry const* get_extern_symbols() const {
      return view().get_extern_symbols();
    }

    std::string_view get_symbol(program_symbol_table_entry const& entry) const {
      return view().get_symbol(entry);
    }

    const_program_view view() const {
      return const_program_view(data);
    }

    program_view view() {
      return program_view(data);
    }

    std::vector<uint8_t> data;
  };
}
