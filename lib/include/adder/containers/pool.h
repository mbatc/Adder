#pragma once

#include <vector>
#include <assert.h>

namespace adder {
  template <typename T>
  class pool {
  public:
    class iterator {
    public:
      iterator(size_t startIndex, pool* pPool)
        : m_pPool(pPool), m_index(startIndex) {}

      T& operator*() { return m_pPool->get(m_index); }
      bool operator==(iterator const& o) const { return m_index == o.m_index && m_pPool == o.m_pPool; }
      bool operator!=(iterator const& o) const { return !(*this == o); }

      iterator& operator++() {
        do {
          ++m_index;
        } while (m_index < m_pPool->capacity() && !m_pPool->used(m_index));
        return *this;
      }

    private:
      pool* m_pPool = nullptr;
      size_t m_index = 0;
    };

    class const_iterator {
    public:
      const_iterator(size_t index, pool const* pPool)
        : m_pPool(pPool), m_index(index) {}

      T const& operator*() { return m_pPool->get(m_index); }
      bool operator==(const_iterator const& o) const { return m_index == o.m_index && m_pPool == o.m_pPool; }
      bool operator!=(const_iterator const& o) const { return !(*this == o); }

      const_iterator& operator++() {
        do {
          ++m_index;
        } while (m_index < m_pPool->capacity() && !m_pPool->used(m_index));
        return *this;
      }

    private:
      pool const* m_pPool = nullptr;
      size_t m_index = 0;
    };

    pool(size_t initialCapacity = 0) {
      reserve(initialCapacity);
    }

    size_t size() const {
      return m_size;
    }

    size_t capacity() const {
      return m_capacity;
    }

    bool used(size_t index) const {
      return index < capacity() && m_used[index];
    }

    size_t insert(T const& value) {
      return emplace(value);
    }

    size_t insert(T&& value) {
      return emplace(std::move(value));
    }

    template <typename... Args>
    size_t emplace(Args&&... args) {
      size_t index = m_size;
      if (m_freed.size() > 0) {
        index = m_freed.back();
        m_freed.pop_back();
      }

      size_t requiredSize = index + 1;
      if (requiredSize > m_capacity) {
        reserve(requiredSize * 2);
      }
      new (m_data + index) T(std::forward<Args>(args)...);
      m_used[index] = true;

      ++m_size;

      return index;
    }

    T& get(size_t index) {
      return m_data[index];
    }

    T const& get(size_t index) const {
      return m_data[index];
    }

    T& operator[](size_t index) {
      return get(index);
    }

    T const& operator[](size_t index) const {
      return get(index);
    }

    T* try_get(size_t index) {
      return used(index) ? data() + index : nullptr;
    }

    T const* try_get(size_t index) const {
      return used(index) ? data() + index : nullptr;
    }

    bool erase(size_t index) {
      if (index >= m_used.size() || !m_used[index])
        return false;

      m_used[index] = false;
      m_freed.push_back(index);
      m_data[index].~T();
      --m_size;
      return true;
    }

    bool reserve(size_t newCapacity) {
      if (newCapacity <= m_capacity) {
        return false;
      }

      T* pNewBuffer = (T*)std::malloc(newCapacity * sizeof(T));
      if (m_data != pNewBuffer) {
        for (size_t i = 0; i < m_capacity; ++i) {
          if (m_used[i]) {
            new (pNewBuffer + i) T(std::move(m_data[i]));
            m_data[i].~T();
          }
        }

        std::swap(pNewBuffer, m_data);
        std::free(pNewBuffer);
      }

      m_capacity = newCapacity;
      m_used.resize(newCapacity, false);
      return true;
    }

    iterator begin() { return iterator(first(), this); }
    iterator end() { return iterator(last(), this); }

    const_iterator begin() const { return const_iterator(first(), this); }
    const_iterator end() const { return const_iterator(last(), this); }

    T const* data() const {
      return m_data;
    }

    T* data() {
      return m_data;
    }

    void clear() {
      for (size_t i = 0; i < m_used.size(); ++i) {
        if (m_used[i]) {
          m_data[i].~T();
          m_used[i] = false;
        }
      }
      m_freed.clear();
      m_size = 0;
    }

  private:
    int64_t first() const {
      if (m_size == 0)
        return 0;
      int64_t i = 0;
      while (!m_used[i])
        ++i;
      return i;
    }

    int64_t last() const {
      if (m_size == 0)
        return 0;

      return m_capacity;
    }

    size_t m_size = 0;
    size_t m_capacity = 0;

    T * m_data = nullptr;

    std::vector<size_t> m_freed;
    std::vector<bool> m_used;
  };
}
