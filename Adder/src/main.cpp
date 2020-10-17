#include "Adder.h"

int main(int argc, char **argv)
{
  // Simple program that adds and multiplies some numbers
  const char *code =
    "load_val 0A    R00 \n" // Load '10' into register 0
    "load_val 05    R01 \n" // Load '5' into register 1
    "add      R00   R01\n" // Add register 0 and 1, storing the result in 1
    "move     RST   R03\n" // Store the current stock offset
    "alloc    04       \n" // Allocate 4 bytes on the stack
    "store    R01   R03\n" // Store register 0 into the allocated memory
    "add      R00   R01\n" // Add register 0 and 1, storing the result in 1
    "add      R00   R01\n" // Add register 0 and 1, storing the result in 1
    "add      R00   R01\n" // Add register 0 and 1, storing the result in 1
    "store    R03   R00\n" // Load the value of the address in R03 into R00
    "mul      R00   R01\n" // Multiple register 0 and 1, storing the result in 1
    "move     RST   R03\n"
    "alloc    04       \n"
    "store    R01   R03\n";

  AdderPrgm prgm = { 0 };
  AdderPrgm_LoadAssembly(&prgm, code);
  AdderPrgm_Run(&prgm);

  system("pause");
}
