#pragma once
#include <stdint.h>
#include "ir_gen.h"
#include "util.h"

typedef struct {
  HashMap offsets;
  uint16_t current_offsets;
  List sysv_code;
} Environment;

typedef struct {
  uint16_t offset;
  uint16_t size;
} SizedOffset;

typedef struct {
  enum {
    SysVLoadMemory,
    SysVStringLiteral,
    SysVImmediate,
    SysVLoadRef,
    SysVGetVarRef,
    SysVLoadVarRef,
    SysVLoadVar,
    SysVVar,
    SysVEnter,
    SysVLeave,
    SysVMovReg,
    SysVAsmCall,
    SysVAsmInline
  } tag;

  union {
    SizedOffset SysVLoadMemory;

    struct SysVStringLiteral {
      String string; // Non-zero terminated
      uint16_t size;
    } SysVStringLiteral;

    uint64_t SysVImmediate;

    struct SysVLoadRef {
      String identifier;
    } SysVLoadRef;

    struct SysVGetVarRef {
      String identifier;
    } SysVGetVarRef;

    SizedOffset SysVLoadVarRef;

    struct SysVLoadVar {
      String identifier;
    } SysVLoadVar;

    struct SysVVar {
      String identifier;
      uint16_t offset;
      uint16_t size;
    } SysVVar;

    struct SysVEnter {
      String label;
      uint16_t reserved_bytes;
    } SysVEnter;

    struct SysVMovReg {
      char *from;
      char *to;
    } SysVMovReg;

    struct SysVAsmCall {
      char *identifier;
    } SysVAsmCall;

    struct SysVAsmInline {
      char *as;
    } SysVAsmInline;
  } data;
} SysV;

typedef struct {
  enum {
    AX,
    BX,
    CX,
    DX,
    BP
  } tag;

  uint16_t size;
  uint16_t offset;
} Register;

void show_reg(Register reg);
void append_sysv(Environment *env, IR ir);
void _ir_to_sysv_env(List *ir, Environment *env);
List ir_to_sysv(List *ir);
void output_sysv(List sysv);