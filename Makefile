CROSS_COMPILE =
CFLAGS = -Os -Wall
TEST_DIR = ./tests
TEST_SRC = $(wildcard $(TEST_DIR)/*.c)
TEST_OBJ = $(TEST_SRC:.c=.o)
PASS_COLOR = \x1b[32;01m
NO_COLOR = \x1b[0m

BIN = amacc

ifeq ($(PREFIX), arm-linux-gnueabihf-)
	CROSS_COMPILE = arm-linux-gnueabihf
else
	CROSS_COMPILE = arm-linux-gnueabi
endif

all: $(BIN)

amacc: amacc.c
	$(CROSS_COMPILE)-gcc $(CFLAGS) -fsigned-char -o amacc amacc.c -g -ldl

check: $(BIN) $(TEST_OBJ)
	@echo "[ compiled ]"
	@qemu-arm -L /usr/$(CROSS_COMPILE) ./amacc tests/hello.c
	@echo "[ nested ]"
	@qemu-arm -L /usr/$(CROSS_COMPILE) ./amacc amacc.c tests/hello.c
	@cloc --quiet amacc.c 2>/dev/null

$(TEST_DIR)/%.o: $(TEST_DIR)/%.c $(BIN)
	@echo "[********* test  $<******* ]"
	@qemu-arm -L /usr/$(CROSS_COMPILE) ./amacc $< 2 
	@/bin/echo -e "$(PASS_COLOR)$< pass$(NO_COLOR)\n"

clean:
	$(RM) $(BIN)
