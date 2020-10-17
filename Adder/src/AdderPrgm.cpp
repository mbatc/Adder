#include "AdderPrgm.h"
#include "AdderParser.h"
#include "AdderProcessor.h"

// Adds an instruction to the program, returns a pointer to the new instruction
void AdderPrgm_AddInstruction(AdderPrgm *pPrgm, AdderInstruction **ppInstr)
{
  if (pPrgm->prgmSize >= pPrgm->prgmCapacity)
  { // We need to allocate more space for instructions
    int newCap = max(1, pPrgm->prgmSize) * 2;
    AdderInstruction *pNewMem = adMalloc_zero_t(AdderInstruction, newCap);
    if (pPrgm->pInstructions)
    {
      ADDER_MEMCPY(pNewMem, pPrgm->pInstructions, pPrgm->prgmSize * sizeof(AdderInstruction));
      free(pPrgm->pInstructions);
    }

    pPrgm->pInstructions = pNewMem;
    pPrgm->prgmCapacity = newCap;
  }

  *ppInstr = &pPrgm->pInstructions[pPrgm->prgmSize];
  AdderInstruction_Create(*ppInstr);
  pPrgm->prgmSize += 1;
}

void AdderPrgm_AddInstruction(AdderPrgm *pPrgm, AdderInstructionCode code, REGISTER_TYPE in0, REGISTER_TYPE in1, REGISTER_TYPE in2)
{
  AdderInstruction *pInst;
  AdderPrgm_AddInstruction(pPrgm, &pInst);
  pInst->code = code;
  pInst->in0 = in0;
  pInst->in1 = in1;
  pInst->in2 = in2;
}

void AdderPrgm_FreeInstructions(AdderPrgm *pPrgm)
{
  for (int i = 0; i < pPrgm->prgmSize; ++i)
    AdderInstruction_Destroy(&pPrgm->pInstructions[i]);
  free(pPrgm->pInstructions);
  pPrgm->pInstructions = 0;
  pPrgm->prgmCapacity = 0;
  pPrgm->prgmSize = 0;
}

// Load a program from byte code
// This byte code uses the instruction specification below, with parameters in hexadecimal format
void AdderPrgm_LoadAssembly(AdderPrgm *pProgram, const char *byteCode)
{
  AdderInstructionCode code;
  REGISTER_TYPE params[3];
  ADDER_MEMSET(params, 0, sizeof(params));
  while (AdderParser_ReadInstruction(&byteCode, &code, params))
  {
    AdderPrgm_AddInstruction(pProgram, code, params[0], params[1], params[2]);
    ADDER_MEMSET(params, 0, sizeof(params));
  }
}

void AdderPrgm_LoadSource(AdderPrgm *pProgram, const char *source)
{
  char *assembly = 0;
  AdderParser_GenerateAssembly((const char**)&assembly, source);
  AdderPrgm_LoadAssembly(pProgram, assembly);
  free(assembly);
}

void AdderPrgm_Run(AdderPrgm *pProgram)
{
  AdderProcessor processor;
  ADDER_MEMSET(&processor, 0, sizeof(AdderProcessor));
  processor.pPrgm = pProgram;

  while (AdderProcessor_Fetch(&processor))
    AdderProcessor_Execute(&processor);
}
