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

Type *create_type(String str);
uint8_t type_size(Type t);
uint8_t strcmp_n(const char *s1, const char *s2, uint16_t n);

typedef struct AST {
  enum {
    ASTExtern,
    ASTFunc,
    ASTCCall,
    ASTVar,
    ASTCall,
    ASTVarRef,
    ASTRef,
    ASTInline,
    ASTDeref,
    ASTIntegral,
    ASTStr,
    ASTIf
  } tag;

  union {
    List ASTExtern; // List of extern functions

    struct ASTFunc {
      Type ret_type;
      List param_types; // List of Type
      String identifier;
      List body; // List of AST
    } ASTFunc;

    struct ASTVar {
      String identifier;
      Type *type;
      struct AST *node;
    } ASTVar;
  } data;
} AST;

AST cell_to_ast(Cell *cell);
List parser_to_ast(List *ast);
