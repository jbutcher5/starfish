CC      = clang++
INCLUDE = app
OBJ     = build
SRC     = app
SRCS    = $(SRC)/main.cpp
SRCS    += $(shell find $(SRC) -type f -name '*.cpp')
OBJS    = $(patsubst $(SRC)/%.cpp,$(OBJ)/%.o,$(SRCS))
EXE     = starc++
CFLAGS  = -I$(INCLUDE) -std=c++20
LDLIBS  =

.PHONY: clean

$(OBJ)/%.o: $(SRC)/%.cpp
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
