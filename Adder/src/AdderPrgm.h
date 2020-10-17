#ifndef AdderPrgm_h__
#define AdderPrgm_h__

#include "AdderInstruction.h"
#include "AdderPrgmMemory.h"

struct AdderPrgm
{
  AdderInstruction *pInstructions; // Program instructions
  int prgmSize;                    // Number of instructions
  int prgmCapacity;                // Number of instructions allocated

  AdderPrgmMemory memory;          // Program memory section
};

// Adds an instruction to the program, returns a pointer to the new instruction
void AdderPrgm_AddInstruction(AdderPrgm *pPrgm, AdderInstruction **ppInstr);
void AdderPrgm_AddInstruction(AdderPrgm *pPrgm, AdderInstructionCode code, REGISTER_TYPE in0, REGISTER_TYPE in1, REGISTER_TYPE in2);
void AdderPrgm_FreeInstructions(AdderPrgm *pPrgm);

// Load a program from byte code
// This byte code uses the instruction specification below, with parameters in hexadecimal format
void AdderPrgm_LoadAssembly(AdderPrgm *pProgram, const char *byteCode);
void AdderPrgm_LoadSource(AdderPrgm *pProgram, const char *source);
void AdderPrgm_Run(AdderPrgm *pProgram);

#endif // AdderPrgm_h__