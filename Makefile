comp: comp.o
	gcc -no-pie -nostartfiles -o comp comp.o

comp.o: comp.asm
	yasm -f elf64 -o comp.o comp.asm

comp.asm: app/Compile.hs
	cabal run exes -- c comptest.scm
