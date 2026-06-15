#pragma once
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include "parse.h"
#include "util.h"

// Reference: https://keleshev.com/abstract-syntax-tree-an-example-in-c/

typedef struct {
  List ir_nodes;
  List types;
  List lists;
} IRAlloc;

enum TypeTag { TYPETAG_PRIMITIVE, TYPETAG_POINTER };
enum Primitive { PRIMITIVE_VOID, PRIMITIVE_INTEGRAL, PRIMITIVE_CHAR };

typedef struct Type {
  enum TypeTag tag;
  union {
      enum Primitive primitive;
      uint32_t deref;
  } detail;
} Type;

typedef struct TypedIdent {
  String ident;
  uint32_t type;
} TypedIdent;

typedef struct FuncSignature {
  uint32_t ret_type;
  uint32_t param_types; // List of TypedIdent
  String identifier;
} FuncSignature;

Type create_type(String str);
uint8_t type_size(Type t);
uint8_t strcmp_n(const char *s1, const char *s2, uint16_t n);

typedef struct IR {
  enum {
    IRExtern,
    IRFunc,
    IRCCall,
    IRVar,
    IRCall,
    IRVarRef,
    IRRef,
    IRInline,
    IRDeref,
    IRInt,
    IRString,
    IRIf
  } tag;

  union {
    struct IRFunc {
      FuncSignature type;
      uint32_t body; // List of IR
    } IRFunc;

    FuncSignature IRCCall; 

    struct IRVar {
      String identifier;
      uint32_t type;
      uint32_t node;
    } IRVar;

    struct IRIf {
      uint32_t condition;
      uint32_t a;
      uint32_t b;
    } IRIf;

    String IRVarRef;

    uint64_t IRInt;
    String IRString;
    
  } data;
} IR;

IR* cell_to_ir(Cell *cell);
List ast_to_ir(List *ast);
