CFLAGS = -O0 -Wall -Wno-misleading-indentation
OBJ_DIR = elf
TEST_DIR = tests
TEST_SRC = $(wildcard $(TEST_DIR)/*.c)
TEST_OBJ = $(TEST_SRC:.c=.o)

BIN = amacc

include mk/arm.mk
include mk/common.mk
all: $(BIN)

amacc: amacc.c
	$(VECHO) "  CC+LD\t\t$@\n"
	$(Q)$(ARM_CC) $(CFLAGS) -o $@ $< -g -ldl

check: $(BIN) $(TEST_OBJ)
	$(VECHO) "[ JIT      ]\n"
	$(Q)$(ARM_EXEC) ./amacc tests/hello.c
	$(VECHO) "[ compiled ]\n"
	$(Q)$(ARM_EXEC) ./amacc -o $(OBJ_DIR)/hello tests/hello.c
	$(Q)$(ARM_EXEC) $(OBJ_DIR)/hello
	$(VECHO) "[ nested   ]\n"
	$(Q)$(ARM_EXEC) ./amacc amacc.c tests/hello.c

$(OBJ_DIR)/amacc: $(BIN)
	$(VECHO) "  SelfCC\t\t$@\n"
	$(Q)$(ARM_EXEC) ./$^ -o $@ amacc.c

SHELL_HACK := $(shell mkdir -p $(OBJ_DIR))
$(TEST_DIR)/%.o: $(TEST_DIR)/%.c $(BIN) $(OBJ_DIR)/amacc
	$(VECHO) "[*** verify $< <JIT>********]\n"
	$(Q)$(ARM_EXEC) ./$(BIN) $< 2
	$(VECHO) "[*** verify $< <ELF>********]\n"
	$(Q)$(ARM_EXEC) ./$(BIN) -o $(OBJ_DIR)/$(notdir $(basename $<)) $<
	$(Q)$(ARM_EXEC) $(OBJ_DIR)/$(notdir $(basename $<)) 2
	$(VECHO) "[*** verify $< <ELF-self>***]\n"
	$(Q)$(ARM_EXEC) ./$(OBJ_DIR)/amacc $< 2
	$(VECHO) "$(PASS_COLOR)$< pass$(NO_COLOR)\n"

clean:
	$(RM) $(BIN) $(OBJ_DIR)/* \
              out-1 out-2
