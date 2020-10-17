#include "AdderParser.h"
#include "AdderScan.h"
#include "AdderSeek.h"

bool AdderParser_ReadInstruction(const char **pByteCode, AdderInstructionCode *pCode, REGISTER_TYPE *pParams)
{
  if (!AdderSeek_SkipSet(pByteCode, ADDER_WHITESPACE))
    return false; // No more instructions

  const char *cursor = *pByteCode; // Store the currect location before seeking
  if (!AdderSeek_SeekToSet(pByteCode, ADDER_WHITESPACE))
    return false;

  *pCode = AdderInstructionCode_FromString(cursor, *pByteCode);
  if (*pCode == Adder_Inst_Unknown)
    return false; // Unknown instruction

  for (int i = 0; i < 3; ++i)
  {
    if (!AdderSeek_SkipSet(pByteCode, " \t\v"))
      return false;

    if (**pByteCode == '\n' || **pByteCode == '\r') // If we hit a new line, we have hit the end of this instruction
      return true;

    cursor = *pByteCode;
    AdderSeek_SeekToSet(pByteCode, ADDER_WHITESPACE);
    if (cursor[0] == 'R') // A register
    {
      AdderMachineRegister reg = AdderMachineRegister_FromString(cursor, *pByteCode);
      if (reg == Adder_Reg_Unknown)
        return false; // Unknown register keyword
      pParams[i] = reg;
    }
    else // load hexadecimal value
    {
      int len = 0;
      pParams[i] = (REGISTER_TYPE)AdderScan_HexIntegerFast(cursor, &len);
    }
  }
  return true;
}

bool AdderParser_GenerateAssembly(const char **pAssembly, const char *source)
{
  ADDER_UNUSED(pAssembly, source);
  return false;
}
