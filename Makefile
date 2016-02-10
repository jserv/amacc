CROSS_COMPILE ?= arm-linux-gnueabihf-
CFLAGS = -Os -Wall
TEST_DIR = ./tests
TEST_SRC = $(wildcard $(TEST_DIR)/*.c)
TEST_OBJ = $(TEST_SRC:.c=.o)
PASS_COLOR = \x1b[32;01m
NO_COLOR = \x1b[0m

BIN = amacc

ARM_EXEC = qemu-arm -L /usr/$(shell echo $(CROSS_COMPILE) | sed s'/.$$//')

all: $(BIN)

amacc: amacc.c
	$(CROSS_COMPILE)gcc $(CFLAGS) -fsigned-char -o amacc amacc.c -g -ldl

check: $(BIN) $(TEST_OBJ)
	@echo "[ compiled ]"
	@$(ARM_EXEC) ./amacc tests/hello.c
	@echo "[ nested ]"
	@$(ARM_EXEC) ./amacc amacc.c tests/hello.c

$(TEST_DIR)/%.o: $(TEST_DIR)/%.c $(BIN)
	@echo "[********* test  $<******* ]"
	@$(ARM_EXEC) ./amacc $< 2
	@/bin/echo -e "$(PASS_COLOR)$< pass$(NO_COLOR)\n"

clean:
	$(RM) $(BIN)
