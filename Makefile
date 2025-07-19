out: out.o
	gcc -no-pie -nostartfiles -o out out.o

out.o: out.asm
	yasm -f elf64 -o out.o out.asm

out.asm: app/Compile.hs app/Assembler.hs
	cabal run exes -- c comptest.star
