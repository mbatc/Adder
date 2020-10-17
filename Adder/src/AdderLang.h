#ifndef AdderLang_h__
#define AdderLang_h__

/**
* Instruction Spec
*
* ----------------+------------+--------------+-----------------+-------------+
*  Description    | Code       |      in0     |       in1       |     in2     |
* ----------------+------------+--------------+-----------------+-------------+
*  No Operation   | nop        |       -      |        -        |      -      |
*  Load           | load       |  src reg     |  dst reg        |      -      |
*  Move           | move       |  src reg     |  dst reg        |      -      |
*  Store          | store      |  src reg     |  dst adr        |      -      |
*  Load Value     | load_val   |  src val     |  dst reg        |      -      |
*  Store Value    | store_val  |  src val     |  dst adr        |      -      |
*  Load Indirect  | load_ind   |  src adr     |  dst reg        |      -      |
*  Store Indirect | store_ind  |  src reg     |  dst adr        |      -      |
*  Add            | add        |  src 1 reg   |  src 2 dst reg  |      -      |
*  Subtract       | sub        |  src 1 reg   |  src 2 dst reg  |      -      |
*  Multiply       | mul        |  src 1 reg   |  src 2 dst reg  |      -      |
*  Divide         | div        |  src 1 reg   |  src 2 dst reg  |      -      |
*  Add Float      | addf       |  src 1 reg   |  src 2 dst reg  |      -      |
*  Subtract Float | subf       |  src 1 reg   |  src 2 dst reg  |      -      |
*  Multiply Float | mulf       |  src 1 reg   |  src 2 dst reg  |      -      |
*  Divide Float   | divf       |  src 1 reg   |  src 2 dst reg  |      -      |
*  Convert        | convert    |  src reg     |        -        |      -      |
*  Jump           | jump       |  src reg     |        -        |      -      |
*  Jump Routine   | jumpr      |  src reg     |        -        |      -      |
*  Return         | return     |       -      |        -        |      -      |
*  Push Stack     | push_stack |  src dst     |        -        |      -      |
*  Pop Stack      | pop_stack  |  dst reg     |        -        |      -      |
*  Increment      | inc        |  src dst reg |        -        |      -      |
*  Decrement      | dec        |  src dst reg |        -        |      -      |
*  Branch         | branch     |  cmp flag    |        -        |      -      |
* ----------------+------------+--------------+-----------------+-------------+
*
*/

#include <string.h>
#include <malloc.h>
#include <stdlib.h>
#include <minmax.h>

// Typedefs for built in types
typedef unsigned char adUInt8;
typedef unsigned char adUInt16;
typedef unsigned char adUInt32;
typedef unsigned char adUInt64;
typedef signed char   adInt8;
typedef signed char   adInt16;
typedef signed char   adInt32;
typedef signed char   adInt64;
typedef float         adFloat32;
typedef double        adFloat64;

// Register descriptions
#define REGISTER_TYPE unsigned int
#define REGISTER_SIZE sizeof(REGISTER_TYPE)

// Common character sets
extern const char *adder_whitespace;
extern const char *adder_hexadecimal;

#define ADDER_WHITESPACE adder_whitespace
#define ADDER_HEXADECIMAL adder_hexadecimal

// Program memory block size
#define PRGM_MEM_STACK_SIZE 1024

void* adder_malloc_zero(int size);

// Memory allocation function definitions
#ifndef ADDER_ALLOC // Use malloc by default, but it can be overridden
#define ADDER_ALLOC malloc
#endif

#ifndef ADDER_FREE // Use free by default, but it can be overridden
#define ADDER_FREE free;
#endif

#ifndef ADDER_MEMCPY
#define ADDER_MEMCPY memcpy
#endif

#ifndef ADDER_MEMSET
#define ADDER_MEMSET memset
#endif

#define ADDER_UNUSED(...) __VA_ARGS__


#define adMalloc_t(type, count) ((type *)ADDER_ALLOC(count * sizeof(type)))
#define adMalloc_zero_t(type, count) ((type *)adder_malloc_zero(count * sizeof(type)))

// Virtual machine definitions
enum AdderInstructionCode
{
  Adder_Inst_Unknown = -1,
  Adder_Inst_Nop = 0,   // Do nothing
  Adder_Inst_Move,      // Copy the value of one register to another
  Adder_Inst_Load,      // Load the value at the specified address into a register
  Adder_Inst_Store,     // Load the value of a registered into the specified address
  Adder_Inst_LoadVal,   // Load a constant value into a register
  Adder_Inst_StoreVal,  // Load a constant value into a register
  Adder_Inst_LoadInd,
  Adder_Inst_StoreInd,
  Adder_Inst_Add,       // Add the values in two registers and store it in a third
  Adder_Inst_Sub,       // Subtract the values in two registers and store it in a third
  Adder_Inst_Mul,       // Multiply the values in two registers and store it in a third
  Adder_Inst_Div,       // Divide the values in two registers and store it in a third
  Adder_Inst_AddF,      // Floating point - Add the values in two registers and store it in a third
  Adder_Inst_SubF,      // Floating point - Subtract the values in two registers and store it in a third
  Adder_Inst_MulF,      // Floating point - Multiply the values in two registers and store it in a third
  Adder_Inst_DivF,      // Floating point - Divide the values in two registers and store it in a third
  Adder_Inst_Convert,   // Convert a value from one format to another
  Adder_Inst_Jump,      // Set the program counter to the specified value
  Adder_Inst_JumpR,     // Set the program counter to the specified value and setup a sub-routine
  Adder_Inst_Return,    // Return from a sub-routine
  Adder_Inst_Alloc,     // Allocate memory on the stack
  Adder_Inst_Dealloc,   // Deallocate memory on the stack
  Adder_Inst_PushStack, // Push a registers value to the stack
  Adder_Inst_PopStack,  // Pop a registers value from the stack
  Adder_Inst_End,
  Adder_Inst_Inc,       // Increment the specified register
  Adder_Inst_Dec,       // Decrement the specified register
  Adder_Inst_Branch,    // Branch instruction
  Adder_Inst_Count,
};
extern const char* AdderInstructionCode_Name[Adder_Inst_Count];

enum AdderMachineRegister
{
  Adder_Reg_Unknown = -1,
  Adder_Reg_Counter = 0, // Program stack pointer
  Adder_Reg_Stack,       // Program stack pointer
  Adder_Reg_0,           // General IO registers
  Adder_Reg_1,
  Adder_Reg_2,
  Adder_Reg_3,
  Adder_Reg_4,
  Adder_Reg_5,
  Adder_Reg_6,
  Adder_Reg_7,
  Adder_Reg_Comp,        // Comparison flag register
  Adder_Reg_Return,      // Return address to a caller location
  Adder_Reg_Count,
};
extern const char* AdderMachineRegister_Name[Adder_Reg_Count];

enum AdderMachineComparisonFlag
{
  Adder_CompFlag_Unknown = -1,
  Adder_CompFlag_Less = 0,
  Adder_CompFlag_Equal,
  Adder_CompFlag_Greater,
  Adder_CompFlag_Count,
};

// Adder Language definitions
enum AdderKeyword
{
  Adder_KW_Unknown = -1,
  Adder_KW_If = 0,
  Adder_KW_Else,
  Adder_KW_For,
  Adder_KW_While,
  Adder_KW_Count
};
extern const char* AdderKeyword_Name[Adder_Reg_Count];

// Built-in basic types
enum AdderType
{
  Adder_Type_Unknown = -1,
  Adder_Type_Int8 = 0,
  Adder_Type_Int16,
  Adder_Type_Int32,
  Adder_Type_Int64,
  Adder_Type_UInt8,
  Adder_Type_UInt16,
  Adder_Type_UInt32,
  Adder_Type_UInt64,
  Adder_Type_Float32,
  Adder_Type_Float64,
  Adder_Type_Count
};
extern const char* AdderType_Name[Adder_Type_Count];

AdderInstructionCode AdderInstructionCode_FromString(const char *begin, const char *end);
AdderMachineRegister AdderMachineRegister_FromString(const char *begin, const char *end);
AdderKeyword         AdderKeyword_FromString(const char *begin, const char *end);
AdderType            AdderType_FromString(const char *begin, const char *end);

#endif // AdderLang_h__