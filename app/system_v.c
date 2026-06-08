#include "system_v.h"
#include "ir_gen.h"
#include "util.h"
#include <stdint.h>

extern IRAlloc ir_alloc;
extern SysVAlloc sysv_alloc;

uint32_t djb2_hash(String str) {
  uint32_t hash = 5381;

  for (uint16_t i = 0; i < str.len; i++)
    hash = 33 * hash + str.start[i];

  return hash;
}

void show_reg(Register reg) {
  if (reg.tag == AX) {
    if (reg.size == 8) {
      printf("rax"); 
    }

    else if (reg.size == 4) {
      printf("eax"); 
    }

    else if (reg.size == 2) {
      printf("ax"); 
    }

    else if (reg.size == 1) {
      printf("al");
    }
  }

  else if (reg.tag == BX) {
    if (reg.size == 8) {
      printf("rbx"); 
    }

    else if (reg.size == 4) {
      printf( "ebx"); 
    }

    else if (reg.size == 2) {
      printf("bx"); 
    }

    else if (reg.size == 1) {
      printf("bl");
    }
  }

  else if (reg.tag == CX) {
    if (reg.size == 8) {
      printf("rcx"); 
    }

    else if (reg.size == 4) {
      printf("ecx"); 
    }

    else if (reg.size == 2) {
      printf("cx"); 
    }

    else if (reg.size == 1) {
      printf("cl");
    }
  }

  else if (reg.tag == DX) {
    if (reg.size == 8) {
      printf("rdx"); 
    }

    else if (reg.size == 4) {
      printf("edx"); 
    }

    else if (reg.size == 2) {
      printf("dx"); 
    }

    else if (reg.size == 1) {
      printf("dl");
    }
  }

  if (reg.tag == BP) {
    printf("[rbp-%d]", reg.offset);
  }
}

void append_sysv(Environment *env, IR ir) {
  if (ir.tag == IRVar) {
    SysV var;
    var.tag = SysVVar;
    var.data.SysVVar.identifier = ir.data.IRVar.identifier;

    Type *t = list_get(&ir_alloc.types, ir.data.IRVar.type);
    IR *node = list_get(&ir_alloc.ir_nodes, ir.data.IRVar.node);

    var.data.SysVVar.size = type_size(*t);
    append_sysv(env, *node);

    env->current_offsets += var.data.SysVVar.size;
    var.data.SysVVar.offset = env->current_offsets;

    SizedOffset memory = {.offset = env->current_offsets, .size = type_size(*t)};

    uint32_t handle = list_push(&sysv_alloc.offets, (void*)&memory);
    SizedOffset *p = list_get(&sysv_alloc.offets, handle);

    HM_Add(&env->offsets,
           djb2_hash(var.data.SysVVar.identifier), (void*)p);
    
    list_push(&env->sysv_code, (void*)&var);
  }

  else if (ir.tag == IRFunc) {
    // Idk maybe just insert params as IRVars

    SysV enter;
    enter.tag = SysVEnter;
    enter.data.SysVEnter.label = ir.data.IRFunc.identifier;

    list_push(&env->sysv_code, (void *)&enter);
    void *enter_sysv = list_get(&env->sysv_code, env->sysv_code.size - 1);

    List *body = list_get(&ir_alloc.lists, ir.data.IRFunc.body);

    _ir_to_sysv_env(body, env);

    ((SysV*)enter_sysv)->data.SysVEnter.reserved_bytes = env->current_offsets;

    SysV leave = {.tag = SysVLeave};
    list_push(&env->sysv_code, (void *)&leave);
  }

  else if (ir.tag == IRIf) {
    SysV if1, if2, if3;

    if1.tag = SysVIfBody1;
    if2.tag = SysVIfBody2;
    if3.tag = SysVIfBody3;

    if1.data.SysVIfBody1 = env->if_index;
    if2.data.SysVIfBody2 = env->if_index;
    if3.data.SysVIfBody3 = env->if_index;

    env->if_index += 2;

    IR *condition = list_get(&ir_alloc.ir_nodes, ir.data.IRIf.condition);
    IR *a = list_get(&ir_alloc.ir_nodes, ir.data.IRIf.a);
    IR *b = list_get(&ir_alloc.ir_nodes, ir.data.IRIf.b);

    append_sysv(env, *condition);
    list_push(&env->sysv_code, (void*)&if1);
    append_sysv(env, *a);
    list_push(&env->sysv_code, (void*)&if2);
    append_sysv(env, *b);
    list_push(&env->sysv_code, (void*)&if3);
  }

  else if (ir.tag == IRVarRef) {
    uint32_t hash = djb2_hash(ir.data.IRVarRef);

    if (!HM_Contains(&env->offsets, hash)) {
      exit(3);
    }

    SizedOffset *memory = HM_Get(&env->offsets, hash);
    
    SysV varref = {.tag = SysVLoadVarRef, .data.SysVLoadVarRef = *memory};

    list_push(&env->sysv_code, (void*)&varref);
  }

  else if (ir.tag == IRInt) {
    SysV immediate = {.tag = SysVImmediate, .data.SysVImmediate = ir.data.IRInt};
    list_push(&env->sysv_code, (void *)&immediate);
  }
}

void _ir_to_sysv_env(List *ir, Environment *env) {
  for (uint32_t i = 0; i < ir->size; i++) {
    append_sysv(env, *(IR *)list_get(ir, i));
  }
}

List ir_to_sysv(List *ir) {
  Environment env = {
    HM_Create(64),
    0,
    list_new(sizeof(SysV)),
    0
  };

  _ir_to_sysv_env(ir, &env);

  HM_Free(&env.offsets);
  
  return env.sysv_code;
}

void output_sysv(List sysv) {
  puts("global main\nsection .note.GNU-stack\nsection .text");

  for (int i = 0; i < sysv.size; i++) {
    SysV *instruction = list_get(&sysv, i);

    if (instruction->tag == SysVVar) {
      uint16_t offset = instruction->data.SysVVar.offset;
      uint16_t size = instruction->data.SysVVar.size;

      printf("\n\tmov [rbp-%d], ", offset);
      show_reg((Register){.tag = AX, .size = size});
    }

    else if (instruction->tag == SysVLoadVarRef) {
      printf("\n\tmov rax, [rbp-%d]", instruction->data.SysVLoadVarRef.offset);
    }

    else if (instruction->tag == SysVImmediate) {
      printf("\n\tmov rax, %d", instruction->data.SysVImmediate);
    }

    else if (instruction->tag == SysVEnter) {
      char buffer[128];
      to_str(instruction->data.SysVEnter.label, buffer, 128);
      
      printf("\n%s:\n\tpush rbp\n\tmov rbp, rsp\n\tsub rsp, %d", buffer, instruction->data.SysVEnter.reserved_bytes);
    }

    else if (instruction->tag == SysVLeave) {
      printf("\n\tmov rsp, rbp\n\tpop rbp\n\tret");
    }

    else if (instruction->tag == SysVIfBody1) {
      printf("\n\tcmp rax, 0");
      printf("\n\tje I%d", instruction->data.SysVIfBody1);
    }

    else if (instruction->tag == SysVIfBody2) {
      printf("\n\tjmp I%d", instruction->data.SysVIfBody2 + 1);
      printf("\nI%d:", instruction->data.SysVIfBody2);
    }

    else if (instruction->tag == SysVIfBody3) {
      printf("\nI%d:", instruction->data.SysVIfBody3 + 1);
    }
  }
}