#ifndef AdderParser_h__
#define AdderParser_h__

#include "AdderInstruction.h"

// Assembly parser
bool AdderParser_ReadInstruction(const char **pByteCode, AdderInstructionCode *pCode, REGISTER_TYPE *pParams);

// Language parser - converts to adder assembly
bool AdderParser_GenerateAssembly(const char **pAssembly, const char *source);

#endif // AdderParser_h__
