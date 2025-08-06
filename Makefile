out: out.o
	gcc -g -no-pie -o out out.o

out.o: out.asm
	nasm -g -f elf64 -o out.o out.asm

out.asm: app/Main.hs app/AST.hs app/Misc.hs app/Compile.hs app/Assembler.hs comptest.star
	cabal run exes -- c comptest.star
