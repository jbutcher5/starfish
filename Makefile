CC      = gcc
INCLUDE = app
OBJ     = build
SRC     = app
SRCS    = $(SRC)/main.c
SRCS    += $(shell find $(SRC) -type f -name '*.c')
OBJS    = $(patsubst $(SRC)/%.c,$(OBJ)/%.o,$(SRCS))
EXE     = starc
CFLAGS  = -I$(INCLUDE) -std=c99
LDLIBS  =

.PHONY: clean

$(OBJ)/%.o: $(SRC)/%.c
	@mkdir -p "$(@D)"
	@echo "Compiling: $< -> $@"
	@$(CC) -c -g $(CFLAGS) $< -o $@

$(EXE): $(OBJS)
	@echo "Building final executable: $@"
	@$(CC) $^ -g $(LDLIBS) -o $@

$(OBJ):
	mkdir -p $@

format: $(SRC)
	clang-format $^ -i

clean:
	rm -rf $(OBJ) $(EXE)
