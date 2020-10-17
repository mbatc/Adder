#include "AdderPrgmMemory.h"

void AdderPrgmMemory_EnsureValid(AdderPrgmMemory *pMem, int pos)
{
  // Check if we need to allocate more blocks to accommodate 'pos'
  int block = pos / PRGM_MEM_STACK_SIZE;
  if (block < pMem->numBlocks)
    return;

  // Allocate pointers to blocks
  char **pNewBlocks = (char**)ADDER_ALLOC(block + 1);
  ADDER_MEMCPY(pNewBlocks, pMem->pBlocks, pMem->numBlocks);
  free(pMem->pBlocks);

  // Allocate new stack blocks
  pMem->pBlocks = pNewBlocks;
  while (block >= pMem->numBlocks)
    pMem->pBlocks[pMem->numBlocks++] = (char*)adder_malloc_zero(PRGM_MEM_STACK_SIZE);
}

bool AdderPrgmMemory_Fetch(AdderPrgmMemory *pMem, unsigned int pos, void *pDst, int size)
{
  int blockIdx = pos / PRGM_MEM_STACK_SIZE;
  int blockOff = pos % PRGM_MEM_STACK_SIZE;

  while (size > 0)
  {
    if (blockIdx >= pMem->numBlocks)
      return false;

    int segSize = min(size, (PRGM_MEM_STACK_SIZE - blockOff));
    ADDER_MEMCPY(pDst, &pMem->pBlocks[blockIdx][blockOff], segSize);
    size -= segSize;
    blockOff = 0;
    ++blockIdx;
  }

  return true;
}

bool AdderPrgmMemory_Store(AdderPrgmMemory *pMem, unsigned int pos, void *pSrc, int size)
{
  int blockIdx = pos / PRGM_MEM_STACK_SIZE;
  int blockOff = pos % PRGM_MEM_STACK_SIZE;

  while (size > 0)
  {
    if (blockIdx >= pMem->numBlocks)
      return false;

    int segSize = min(size, (PRGM_MEM_STACK_SIZE - blockOff));
    ADDER_MEMCPY(&pMem->pBlocks[blockIdx][blockOff], pSrc, segSize);
    size -= segSize;
    blockOff = 0;
    ++blockIdx;
  }

  return true;
}

void AdderPrgmMemory_Fetch(AdderPrgmMemory *pMem, int pos, REGISTER_TYPE *pDst)
{
  AdderPrgmMemory_Fetch(pMem, pos, pDst, REGISTER_SIZE);
}

void AdderPrgmMemory_Store(AdderPrgmMemory *pMem, REGISTER_TYPE val, int pos)
{
  AdderPrgmMemory_Store(pMem, pos, &val, REGISTER_SIZE);
}

void AdderPrgmMemory_FreeAll(AdderPrgmMemory *pMem)
{
  for (int i = 0; i < pMem->numBlocks; ++i)
    free(pMem->pBlocks[i]);
  free(pMem->pBlocks);
  pMem->pBlocks = 0;
}