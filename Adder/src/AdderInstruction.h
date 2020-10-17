#ifndef AdderInstruction_h__
#define AdderInstruction_h__

#include "AdderLang.h"

struct AdderInstruction
{
  AdderInstructionCode code;                    // Instruction code
  REGISTER_TYPE in0; // Instruction Inputs
  REGISTER_TYPE in1;
  REGISTER_TYPE in2;
};

void AdderInstruction_Create(AdderInstruction *pInst);
void AdderInstruction_Destroy(AdderInstruction *pInst);

#endif // AdderInstruction_h__
