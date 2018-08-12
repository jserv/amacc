CFLAGS = -O0 -Wall -Wno-misleading-indentation
OBJ_DIR = elf
TEST_DIR = tests
TEST_SRC = $(wildcard $(TEST_DIR)/*.c)
TEST_OBJ = $(TEST_SRC:.c=.o)

BIN = amacc
EXEC = $(BIN) $(BIN)-native

include mk/arm.mk
include mk/common.mk
all: $(EXEC)

$(BIN): $(BIN).c
	$(VECHO) "  CC+LD\t\t$@\n"
	$(Q)$(ARM_CC) $(CFLAGS) -o $@ $< -g -ldl

$(BIN)-native: $(BIN).c
	$(VECHO) "  CC+LD\t\t$@\n"
	$(Q)$(CC) $(CFLAGS) -o $@ $< \
	    -Wno-pointer-to-int-cast -Wno-int-to-pointer-cast -Wno-format \
	    -ldl

check: $(EXEC) $(TEST_OBJ)
	$(VECHO) "[ C to IR  ]\n"
	$(Q)./$(BIN)-native -s tests/arginc.c | diff tests/arginc.list -
	$(VECHO) "[ JIT      ]\n"
	$(Q)$(ARM_EXEC) ./$(BIN) tests/hello.c
	$(VECHO) "[ compiled ]\n"
	$(Q)$(ARM_EXEC) ./$(BIN) -o $(OBJ_DIR)/hello tests/hello.c
	$(Q)$(ARM_EXEC) $(OBJ_DIR)/hello
	$(VECHO) "[ nested   ]\n"
	$(Q)$(ARM_EXEC) ./$(BIN) $(BIN).c tests/hello.c
	$(VECHO) "[ Compatibility with GCC ]\n"
	$(Q)python runtest.py || echo

$(OBJ_DIR)/$(BIN): $(BIN)
	$(VECHO) "  SelfCC\t\t$@\n"
	$(Q)$(ARM_EXEC) ./$^ -o $@ $(BIN).c

SHELL_HACK := $(shell mkdir -p $(OBJ_DIR))
$(TEST_DIR)/%.o: $(TEST_DIR)/%.c $(BIN) $(OBJ_DIR)/$(BIN)
	$(VECHO) "[*** verify $< <JIT>********]\n"
	$(Q)$(ARM_EXEC) ./$(BIN) $< 2
	$(VECHO) "[*** verify $< <ELF>********]\n"
	$(Q)$(ARM_EXEC) ./$(BIN) -o $(OBJ_DIR)/$(notdir $(basename $<)) $<
	$(Q)$(ARM_EXEC) $(OBJ_DIR)/$(notdir $(basename $<)) 2
	$(VECHO) "[*** verify $< <ELF-self>***]\n"
	$(Q)$(ARM_EXEC) ./$(OBJ_DIR)/$(BIN) $< 2
	$(VECHO) "$(PASS_COLOR)$< pass$(NO_COLOR)\n"

clean:
	$(RM) $(EXEC) $(OBJ_DIR)/* \
              out-1 out-2
