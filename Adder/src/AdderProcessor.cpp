#include "AdderProcessor.h"

void AdderProcessor_Set(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst)
{
  pProcessor->reg[dst] = pProcessor->reg[src];
}

void AdderProcessor_Load(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst)
{
  AdderPrgmMemory_Fetch(&pProcessor->pPrgm->memory, pProcessor->reg[src], &pProcessor->reg[dst]);
}

void AdderProcessor_LoadIndirect(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst)
{
  REGISTER_TYPE srcAdr;
  AdderPrgmMemory_Fetch(&pProcessor->pPrgm->memory, pProcessor->reg[src], &srcAdr);
  AdderPrgmMemory_Fetch(&pProcessor->pPrgm->memory, srcAdr, &pProcessor->reg[dst]);
}

void AdderProcessor_LoadVal(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst)
{
  pProcessor->reg[dst] = src;
}

void AdderProcessor_StoreVal(AdderProcessor *pProcessor, REGISTER_TYPE val, REGISTER_TYPE dst)
{
  AdderPrgmMemory_Store(&pProcessor->pPrgm->memory, val, pProcessor->reg[dst]);
}

void AdderProcessor_Store(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst)
{
  AdderProcessor_StoreVal(pProcessor, pProcessor->reg[src], dst);
}

void AdderProcessor_StoreIndirect(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst)
{
  REGISTER_TYPE dstAdr;
  AdderPrgmMemory_Fetch(&pProcessor->pPrgm->memory, pProcessor->reg[dst], &dstAdr);
  AdderPrgmMemory_Store(&pProcessor->pPrgm->memory, dstAdr, pProcessor->reg[src]);
}

void AdderProcessor_Add(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst)      { pProcessor->reg[dst] += pProcessor->reg[src]; }
void AdderProcessor_Subtract(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst) { pProcessor->reg[dst] -= pProcessor->reg[src]; }
void AdderProcessor_Multilpy(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst) { pProcessor->reg[dst] *= pProcessor->reg[src]; }
void AdderProcessor_Divide(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst)   { pProcessor->reg[dst] /= pProcessor->reg[src]; }

void AdderProcessor_AddFloat(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst)      { *((float*)&pProcessor->reg[dst]) += *(float*)&pProcessor->reg[src]; }
void AdderProcessor_SubtractFloat(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst) { *((float*)&pProcessor->reg[dst]) -= *(float*)&pProcessor->reg[src]; }
void AdderProcessor_MultilpyFloat(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst) { *((float*)&pProcessor->reg[dst]) *= *(float*)&pProcessor->reg[src]; }
void AdderProcessor_DivideFloat(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE dst)   { *((float*)&pProcessor->reg[dst]) /= *(float*)&pProcessor->reg[src]; }

void AdderProcessor_Convert(AdderProcessor *pProcessor, REGISTER_TYPE src, REGISTER_TYPE toType, REGISTER_TYPE fromType)
{
  ADDER_UNUSED(pProcessor, src, toType, fromType);

  /*
  switch (toType)
  {
  case Adder_Type_Int8:
    switch (fromType)
    {
    case Adder_Type_Int8:
    case Adder_Type_Int16:
    case Adder_Type_Int32:
    case Adder_Type_Int64:
    case Adder_Type_Float32:
    case Adder_Type_Float64:
    }
  case Adder_Type_Int16:
  case Adder_Type_Int32:
  case Adder_Type_Int64:
  case Adder_Type_Float32:
  case Adder_Type_Float64:
  }*/
}

void AdderProcessor_Jump(AdderProcessor *pProcessor, REGISTER_TYPE src)
{
  pProcessor->reg[Adder_Reg_Counter] = pProcessor->reg[src];
}

void AdderProcessor_AllocStack(AdderProcessor *pProcessor, REGISTER_TYPE size)
{
  AdderPrgmMemory_EnsureValid(&pProcessor->pPrgm->memory, pProcessor->reg[Adder_Reg_Stack] + size);
  pProcessor->reg[Adder_Reg_Stack] += size;
}

void AdderProcessor_DeallocStack(AdderProcessor *pProcessor, REGISTER_TYPE size)
{
  pProcessor->reg[Adder_Reg_Stack] -= size;
}

void AdderProcessor_PopStack(AdderProcessor *pProcessor, REGISTER_TYPE dst)
{
  AdderPrgmMemory_Fetch(&pProcessor->pPrgm->memory, pProcessor->reg[Adder_Reg_Stack] - REGISTER_SIZE, &pProcessor->reg[dst]);
  AdderProcessor_DeallocStack(pProcessor, REGISTER_SIZE);
}

void AdderProcessor_PushStack(AdderProcessor *pProcessor, REGISTER_TYPE src)
{
  REGISTER_TYPE pos = pProcessor->reg[Adder_Reg_Stack];
  AdderProcessor_AllocStack(pProcessor, REGISTER_SIZE);
  AdderPrgmMemory_Store(&pProcessor->pPrgm->memory, pos, pProcessor->reg[src]);
}

void AdderProcessor_JumpRoutine(AdderProcessor *pProcessor, REGISTER_TYPE src)
{
  AdderProcessor_PushStack(pProcessor, Adder_Reg_Return);
  AdderProcessor_Set(pProcessor, Adder_Reg_Counter, Adder_Reg_Return);
  AdderProcessor_Set(pProcessor, src, Adder_Reg_Counter);
}

void AdderProcessor_Return(AdderProcessor *pProcessor)
{
  AdderProcessor_PopStack(pProcessor, Adder_Reg_Return);
  AdderProcessor_Set(pProcessor, Adder_Reg_Counter, Adder_Reg_Return);
}

void AdderProcessor_Increment(AdderProcessor *pProcessor, REGISTER_TYPE srcDst) { ++pProcessor->reg[srcDst]; }
void AdderProcessor_Decrement(AdderProcessor *pProcessor, REGISTER_TYPE srcDst) { --pProcessor->reg[srcDst]; }

void AdderProcessor_ExecuteInstrSep(AdderProcessor *pProcessor, AdderInstructionCode code, REGISTER_TYPE in0, REGISTER_TYPE in1, REGISTER_TYPE in2)
{
  switch (code)
  {
  case Adder_Inst_Nop: break;
  case Adder_Inst_Load:      AdderProcessor_Load(pProcessor, in0, in1);   break;
  case Adder_Inst_Move:      AdderProcessor_Set(pProcessor, in0, in1);   break;
  case Adder_Inst_Store:     AdderProcessor_Store(pProcessor, in0, in1);  break;
  case Adder_Inst_LoadVal:   AdderProcessor_LoadVal(pProcessor, in0, in1); break;
  case Adder_Inst_StoreVal:  AdderProcessor_StoreVal(pProcessor, in0, in1); break;
  case Adder_Inst_LoadInd:   AdderProcessor_LoadIndirect(pProcessor, in0, in1); break;
  case Adder_Inst_StoreInd:  AdderProcessor_StoreIndirect(pProcessor, in0, in1); break;
  case Adder_Inst_Add:       AdderProcessor_Add(pProcessor, in0, in1); break;
  case Adder_Inst_Sub:       AdderProcessor_Subtract(pProcessor, in0, in1); break;
  case Adder_Inst_Mul:       AdderProcessor_Multilpy(pProcessor, in0, in1); break;
  case Adder_Inst_Div:       AdderProcessor_Divide(pProcessor, in0, in1); break;
  case Adder_Inst_Jump:      AdderProcessor_Jump(pProcessor, in0); break;
  case Adder_Inst_JumpR:     AdderProcessor_JumpRoutine(pProcessor, in0); break;
  case Adder_Inst_Return:    AdderProcessor_Return(pProcessor); break;
  case Adder_Inst_Alloc:     AdderProcessor_AllocStack(pProcessor, in0); break;
  case Adder_Inst_Dealloc:   AdderProcessor_DeallocStack(pProcessor, in0); break;
  case Adder_Inst_PushStack: AdderProcessor_PopStack(pProcessor, in0); break;
  case Adder_Inst_PopStack:  AdderProcessor_PushStack(pProcessor, in0); break;
  case Adder_Inst_Inc:       AdderProcessor_Increment(pProcessor, in0); break;
  case Adder_Inst_Dec:       AdderProcessor_Decrement(pProcessor, in0); break;
  }
}

void AdderProcessor_ExecuteInstr(AdderProcessor *pProcessor, AdderInstruction *pInst)
{
  AdderProcessor_ExecuteInstrSep(pProcessor, pInst->code, pInst->in0, pInst->in1, pInst->in2);
}

bool AdderProcessor_Fetch(AdderProcessor *pProcessor)
{
  int pc = pProcessor->reg[Adder_Reg_Counter];
  if (pc >= pProcessor->pPrgm->prgmSize)
    return false;

  // Copy the instruction pointed to by the program counter to the processor
  ADDER_MEMCPY(&pProcessor->instr, &pProcessor->pPrgm->pInstructions[pc], sizeof(AdderInstruction));
  return true;
}

void AdderProcessor_Execute(AdderProcessor *pProcessor)
{
  // Execute the instruction
  AdderProcessor_ExecuteInstr(pProcessor, &pProcessor->instr);

  // Increment the program counter
  AdderProcessor_Increment(pProcessor, Adder_Reg_Counter);
}
