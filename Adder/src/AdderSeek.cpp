#include "AdderSeek.h"

// Skip to the first occur of c
bool AdderSeek_SeekTo(const char **pSrc, char c)
{
  while (**pSrc && **pSrc != c)
    ++(*pSrc);

  return **pSrc != 0;
}

// Skip to the first occur of a character in 'set'
bool AdderSeek_SeekToSet(const char **pSrc, const char *set)
{
  const char *setCursor;
  while (**pSrc)
  {
    setCursor = set;
    while (*setCursor)
    { // Check if character is in the set
      if (*setCursor == **pSrc)
        return true;
      ++setCursor;
    }

    // Increment the cursor
    ++(*pSrc);
  }
  return **pSrc != 0;
}

// Skip to the first occur of a character not in 'set'
bool AdderSeek_SkipSet(const char **pSrc, const char *set)
{
  const char *setCursor;
  while (**pSrc)
  {
    setCursor = set;
    while (*setCursor)
    { // Check if character is in the set
      if (*setCursor == **pSrc)
        break;
      ++setCursor;
    }

    if (!*setCursor)
      return true;

    // Increment the cursor
    ++(*pSrc);
  }

  return **pSrc != 0;
}

// Seek to the first character in the specified range inclusively
bool AdderSeek_SeekToRange(const char **pSrc, char min, char max)
{
  while (**pSrc && **pSrc >= min && **pSrc <= max)
    ++(*pSrc);
  return **pSrc != 0;
}

// Seek to the first character in the specified range exclusively
bool AdderSeek_SkipRange(const char **pSrc, char min, char max)
{
  while (**pSrc && **pSrc > min && **pSrc < max)
    ++(*pSrc);
  return **pSrc != 0;
}
