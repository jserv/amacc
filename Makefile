CROSS_COMPILE =
CFLAGS = -Os -Wall
TEST_DIR = ./tests
TEST_SRC = $(wildcard $(TEST_DIR)/*.c)
TEST_OBJ = $(TEST_SRC:.c=.o)
PASS_COLOR = \x1b[32;01m
NO_COLOR = \x1b[0m

BIN = amacc

all: $(BIN)

amacc: amacc.c
	$(CC) $(CFLAGS) -m32 -fsigned-char -o amacc amacc.c -g -ldl

check: $(BIN) $(TEST_OBJ)
	@echo "[ compiled ]"
	@./amacc tests/hello.c
	@echo "[ nested ]"
	@./amacc amacc.c tests/hello.c
	@cloc --quiet amacc.c 2>/dev/null

$(TEST_DIR)/%.o: $(TEST_DIR)/%.c $(BIN)
	@echo "[********* test  $<******* ]"
	@./amacc $< 2
	@/bin/echo -e "$(PASS_COLOR)$< pass$(NO_COLOR)\n"

clean:
	$(RM) $(BIN)
