out: out.o
	gcc -g -no-pie -nostartfiles -o out out.o

out.o: out.asm
	nasm -g -f elf64 -o out.o out.asm

out.asm: app/Compile.hs app/Assembler.hs
	cabal run exes -- c comptest.star
