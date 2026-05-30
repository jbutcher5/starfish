#include "system_v.h"
#include "ir_gen.h"
#include "util.h"
#include <stdint.h>

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

    else if (reg.size == 2 || reg.size == 1) {
      printf("ax"); 
    }
  }

  else if (reg.tag == BX) {
    if (reg.size == 8) {
      printf("rbx"); 
    }

    else if (reg.size == 4) {
      printf( "ebx"); 
    }

    else if (reg.size == 2 || reg.size == 1) {
      printf("bx"); 
    }
  }

  else if (reg.tag == CX) {
    if (reg.size == 8) {
      printf("rcx"); 
    }

    else if (reg.size == 4) {
      printf("ecx"); 
    }

    else if (reg.size == 2 || reg.size == 1) {
      printf("cx"); 
    }
  }

  else if (reg.tag == DX) {
    if (reg.size == 8) {
      printf("rdx"); 
    }

    else if (reg.size == 4) {
      printf("edx"); 
    }

    else if (reg.size == 2 || reg.size == 1) {
      printf("dx"); 
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
    var.data.SysVVar.size = type_size(*ir.data.IRVar.type);

    append_sysv(env, *ir.data.IRVar.node);

    env->current_offsets += var.data.SysVVar.size;
    var.data.SysVVar.offset = env->current_offsets;

    SizedOffset *memory = malloc(sizeof(SizedOffset));
    memory->offset = env->current_offsets;
    memory->size = type_size(*ir.data.IRVar.type);

    HM_Add(&env->offsets,
           djb2_hash(var.data.SysVVar.identifier), (void*)memory);
    
    list_push(&env->sysv_code, (void*)&var);
  }

  else if (ir.tag == IRFunc) {
    // Idk maybe just insert params as IRVars

    SysV enter;
    enter.tag = SysVEnter;
    enter.data.SysVEnter.label = ir.data.IRFunc.identifier;

    list_push(&env->sysv_code, (void *)&enter);
    void *enter_sysv = list_grab(&env->sysv_code, env->sysv_code.size - 1);

    _ir_to_sysv_env(&ir.data.IRFunc.body, env);

    ((SysV*)enter_sysv)->data.SysVEnter.reserved_bytes = env->current_offsets;

    SysV leave = {.tag = SysVLeave};
    list_push(&env->sysv_code, (void *)&leave);
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
    append_sysv(env, *(IR *)list_grab(ir, i));
  }
}

List ir_to_sysv(List *ir) {
  Environment env = {
    HM_Create(64),
    0,
    list_new(sizeof(SysV))
  };

  _ir_to_sysv_env(ir, &env);

  HM_Free(&env.offsets);
  
  return env.sysv_code;
}

void output_sysv(List sysv) {
  puts("global main\nsection .note.GNU-stack\nsection .text");

  for (int i = 0; i < sysv.size; i++) {
    SysV *instruction = list_grab(&sysv, i);

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
  }
}