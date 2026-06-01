#!/bin/sh

make -j$(nproc)
./starc test2.star > out.asm
nasm -g -f elf64 -o out.o out.asm
gcc -g -no-pie -o out out.o
./out