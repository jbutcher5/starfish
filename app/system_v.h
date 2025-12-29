#include <stdint.h>

typedef struct {
  enum {
    LoadMemory,
    StringLiteral,
    LoadRef,
    GetVarRef,
    LoadVarRef,
    LoadVar,
    Var,
    Enter,
    Leave,
    MovReg,
    AsmCall,
    AsmInline
  } tag;

  union {
    struct LoadMemory {
      uint16_t size;
      uint16_t offset;
    } LoadMemory;

    struct StringLiteral {
      char *string; // Non-zero terminated
      uint16_t size;
    } StringLiteral;

    struct LoadRef {
      char *identifier;
    } LoadRef;

    struct GetVarRef {
      char *identifier;
    } GetVarRef;

    struct LoadVarRef {
      uint16_t size;
      uint16_t offset;
    } LoadVarRef;

    struct LoadVar {
      char *identifier;
    } LoadVar;

    struct Var {
      char *identifier;
      uint16_t offset;
      uint16_t size;
    } Var;

    struct Enter {
      char *label;
      uint16_t reserved_bytes;
    } Enter;

    struct MovReg {
      char *from;
      char *to;
    } MovReg;

    struct AsmCall {
      char *identifier;
    } AsmCall;

    struct AsmInline {
      char *as;
    } AsmInline;
  } data;
} SysV;
