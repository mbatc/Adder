#include "AdderInstruction.h"

void AdderInstruction_Create(AdderInstruction *pInst) { ADDER_MEMSET(pInst, 0, sizeof(AdderInstruction)); }
void AdderInstruction_Destroy(AdderInstruction *pInst) {}
