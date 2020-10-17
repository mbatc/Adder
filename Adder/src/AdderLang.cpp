#include "AdderLang.h"

const char *adder_whitespace = " \r\n\t\v";
const char *adder_hexadecimal = "0123456789ABCDEF";


void* adder_malloc_zero(int size)
{
  void *pMem = ADDER_ALLOC(size);
  ADDER_MEMSET(pMem, 0, size);
  return pMem;
}

AdderInstructionCode AdderInstructionCode_FromString(const char *begin, const char *end)
{
  for (int i = 0; i < Adder_Inst_Count; ++i)
    if (strncmp(begin, AdderInstructionCode_Name[i], end - begin) == 0)
      return AdderInstructionCode(i);
  return Adder_Inst_Unknown;
}

AdderMachineRegister AdderMachineRegister_FromString(const char *begin, const char *end)
{
  for (int i = 0; i < Adder_Reg_Count; ++i)
    if (strncmp(begin, AdderMachineRegister_Name[i], end - begin) == 0)
      return AdderMachineRegister(i);
  return Adder_Reg_Unknown;
}

AdderKeyword AdderKeyword_FromString(const char * begin, const char * end)
{
  for (int i = 0; i < Adder_KW_Count; ++i)
    if (strncmp(begin, AdderKeyword_Name[i], end - begin) == 0)
      return AdderKeyword(i);
  return Adder_KW_Unknown;
}

AdderType AdderType_FromString(const char * begin, const char * end)
{
  for (int i = 0; i < Adder_Type_Count; ++i)
    if (strncmp(begin, AdderType_Name[i], end - begin) == 0)
      return AdderType(i);
  return Adder_Type_Unknown;
}

const char* AdderInstructionCode_Name[Adder_Inst_Count] = {
  "nop",
  "mov",
  "load",
  "store",
  "load_val",
  "store_val",
  "load_ind",
  "store_ind",
  "add",
  "sub",
  "mul",
  "div",
  "addf",
  "subf",
  "mulf",
  "divf",
  "convert",
  "jump",
  "jumpr",
  "return",
  "alloc",
  "dealloc",
  "push_stack",
  "pop_stack",
  "end",
  "inc",
  "dec",
  "branch"
};

const char* AdderMachineRegister_Name[Adder_Reg_Count] = {
  "RCT",
  "RST",
  "R00",
  "R01",
  "R02",
  "R03",
  "R04",
  "R05",
  "R06",
  "R07",
  "RCM",
  "RRT"
};

const char* AdderKeyword_Name[Adder_Reg_Count] = {
  "if",
  "else",
  "for",
  "while"
};

const char* AdderType_Name[Adder_Type_Count] = {
  "int8",
  "int16",
  "int",
  "int64",
  "uint8",
  "uint16",
  "uint",
  "uint64",
  "float",
  "double"
};
