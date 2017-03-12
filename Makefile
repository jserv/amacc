CROSS_COMPILE ?= arm-linux-gnueabihf-
CFLAGS = -O0 -Wall
MKDIR_P = mkdir -p
OBJ_DIR = elf
TEST_DIR = tests
TEST_SRC = $(wildcard $(TEST_DIR)/*.c)
TEST_OBJ = $(TEST_SRC:.c=.o)
PASS_COLOR = \x1b[32;01m
NO_COLOR = \x1b[0m

BIN = amacc

ARM_EXEC = qemu-arm -L /usr/$(shell echo $(CROSS_COMPILE) | sed s'/.$$//')

all: $(OBJ_DIR) $(BIN)

$(OBJ_DIR):
	$(MKDIR_P) $(OBJ_DIR)

amacc: amacc.c
	$(CROSS_COMPILE)gcc $(CFLAGS) -fsigned-char -o amacc $? -g -ldl

check: $(BIN) $(TEST_OBJ)
	@echo "[ JIT      ]"
	@$(ARM_EXEC) ./amacc tests/hello.c
	@echo "[ compiled ]"
	@$(ARM_EXEC) ./amacc -o $(OBJ_DIR)/hello tests/hello.c
	@$(ARM_EXEC) $(OBJ_DIR)/hello
	@echo "[ nested   ]"
	@$(ARM_EXEC) ./amacc amacc.c tests/hello.c

$(OBJ_DIR)/amacc: $(BIN)
	@mkdir -p $(OBJ_DIR)
	@$(ARM_EXEC) ./$^ -o $(OBJ_DIR)/amacc amacc.c

$(TEST_DIR)/%.o: $(TEST_DIR)/%.c $(BIN) $(OBJ_DIR)/amacc
	@echo "[*** verify $< <JIT>********]"
	@$(ARM_EXEC) ./$(BIN) $< 2
	@echo "[*** verify $< <ELF>********]"
	@mkdir -p $(OBJ_DIR)
	@$(ARM_EXEC) ./$(BIN) -o $(OBJ_DIR)/$(notdir $(basename $<)) $<
	@$(ARM_EXEC) $(OBJ_DIR)/$(notdir $(basename $<)) 2
	@echo "[*** verify $< <ELF-self>***]"
	@$(ARM_EXEC) ./$(OBJ_DIR)/amacc $< 2
	@/bin/echo -e "$(PASS_COLOR)$< pass$(NO_COLOR)\n"

clean:
	$(RM) $(BIN) $(OBJ_DIR)/* \
              out-1 out-2
	@rm -rf $(OBJ_DIR)
