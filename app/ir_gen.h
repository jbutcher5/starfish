#pragma once
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include "parse.h"
#include "util.h"

// Reference: https://keleshev.com/abstract-syntax-tree-an-example-in-c/

enum TypeTag { TYPETAG_PRIMITIVE, TYPETAG_POINTER };
enum Primitive { PRIMITIVE_VOID, PRIMITIVE_INTEGRAL, PRIMITIVE_CHAR };

typedef struct Type {
    enum TypeTag tag;
    union {
        enum Primitive primitive;
        struct Type *deref;
    } detail;
} Type;

typedef struct TypedIdent {
    String ident;
    Type *type;
} TypedIdent;

Type *create_type(String str);
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
    List IRExtern; // List of extern functions

    struct IRFunc {
      Type *ret_type;
      List param_types; // List of TypedIdent
      String identifier;
      List body; // List of IR
    } IRFunc;

    struct IRVar {
      String identifier;
      Type *type;
      struct IR *node;
    } IRVar;

    String IRVarRef;

    uint64_t IRInt;
    String IRString;
    
  } data;
} IR;

IR* cell_to_ir(Cell *cell);
List ast_to_ir(List *ast);
