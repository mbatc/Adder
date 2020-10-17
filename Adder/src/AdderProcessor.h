#ifndef AdderProcessor_h__
#define AdderProcessor_h__

#include "AdderPrgm.h"

struct AdderProcessor
{
  AdderInstruction instr;             // Currently loaded instruction
  REGISTER_TYPE reg[Adder_Reg_Count]; // Registers

  AdderPrgm *pPrgm;                   // Active program
};

void AdderProcessor_Set(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst);
void AdderProcessor_Load(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst);
void AdderProcessor_LoadIndirect(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst);
void AdderProcessor_LoadVal(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst);
void AdderProcessor_StoreVal(AdderProcessor *pProcessor, REGISTER_TYPE val, REGISTER_TYPE dst);
void AdderProcessor_Store(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst);
void AdderProcessor_StoreIndirect(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst);

void AdderProcessor_Add(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst);
void AdderProcessor_Subtract(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst);
void AdderProcessor_Multilpy(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst);
void AdderProcessor_Divide(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst);

void AdderProcessor_AddFloat(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst);
void AdderProcessor_SubtractFloat(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst);
void AdderProcessor_MultilpyFloat(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst);
void AdderProcessor_DivideFloat(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst);

void AdderProcessor_Convert(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE toType, REGISTER_TYPE fromType);

void AdderProcessor_Jump(AdderProcessor *pProcessor, REGISTER_TYPE src);
void AdderProcessor_AllocStack(AdderProcessor *pProcessor, REGISTER_TYPE size);
void AdderProcessor_DeallocStack(AdderProcessor *pProcessor, REGISTER_TYPE size);
void AdderProcessor_PopStack(AdderProcessor *pProcessor, REGISTER_TYPE dst);
void AdderProcessor_PushStack(AdderProcessor *pProcessor, REGISTER_TYPE src);
void AdderProcessor_JumpRoutine(AdderProcessor *pProcessor, REGISTER_TYPE src);
void AdderProcessor_Return(AdderProcessor *pProcessor);
void AdderProcessor_Increment(AdderProcessor *pProcessor, REGISTER_TYPE srcDst);
void AdderProcessor_Decrement(AdderProcessor *pProcessor, REGISTER_TYPE srcDst);

void AdderProcessor_ExecuteInstrSep(AdderProcessor *pProcessor, AdderInstructionCode code, REGISTER_TYPE in0, REGISTER_TYPE in1, REGISTER_TYPE in2);
void AdderProcessor_ExecuteInstr(AdderProcessor *pProcessor, AdderInstruction *pInst);

bool AdderProcessor_Fetch(AdderProcessor *pProcessor);
void AdderProcessor_Execute(AdderProcessor *pProcessor);

#endif // AdderProcessor_h__
