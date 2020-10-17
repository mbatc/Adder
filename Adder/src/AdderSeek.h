#ifndef AdderSeek_h__
#define AdderSeek_h__

// Skip to the first occur of c
bool AdderSeek_SeekTo(const char **pSrc, char c);

// Skip to the first occur of a character in 'set'
bool AdderSeek_SeekToSet(const char **pSrc, const char *set);

// Skip to the first occur of a character not in 'set'
bool AdderSeek_SkipSet(const char **pSrc, const char *set);

// Seek to the first character in the specified range inclusively
bool AdderSeek_SeekToRange(const char **pSrc, char min, char max);

// Seek to the first character in the specified range exclusively
bool AdderSeek_SkipRange(const char **pSrc, char min, char max);

#endif // AdderSeek_h__
