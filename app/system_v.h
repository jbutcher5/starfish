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
  enum {
    SysVLoadMemory,
    SysVStringLiteral,
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
    struct SysVLoadMemory {
      uint16_t size;
      uint16_t offset;
    } SysVLoadMemory;

    struct SysVStringLiteral {
      String string; // Non-zero terminated
      uint16_t size;
    } SysVStringLiteral;

    struct SysVLoadRef {
      String identifier;
    } SysVLoadRef;

    struct SysVGetVarRef {
      String identifier;
    } SysVGetVarRef;

    struct SysVLoadVarRef {
      uint16_t size;
      uint16_t offset;
    } SysVLoadVarRef;

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

void append_sysv(Environment *env, IR ir);
List ir_to_sysv(List *ir);
