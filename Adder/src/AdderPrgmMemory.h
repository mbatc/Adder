#ifndef AdderPrgmMemory_h__
#define AdderPrgmMemory_h__

#include "AdderLang.h"

struct AdderPrgmMemory
{
  char **pBlocks;
  int numBlocks;
};

void AdderPrgmMemory_EnsureValid(AdderPrgmMemory *pMem, int pos);
bool AdderPrgmMemory_Fetch(AdderPrgmMemory *pMem, unsigned int pos, void *pDst, int size);
bool AdderPrgmMemory_Store(AdderPrgmMemory *pMem, unsigned int pos, void *pSrc, int size);
void AdderPrgmMemory_Fetch(AdderPrgmMemory *pMem, int pos, REGISTER_TYPE *pDst);
void AdderPrgmMemory_Store(AdderPrgmMemory *pMem, REGISTER_TYPE val, int pos);
void AdderPrgmMemory_FreeAll(AdderPrgmMemory *pMem);

#endif // AdderPrgmMemory_h__
