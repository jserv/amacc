CROSS_COMPILE =
CFLAGS = -Os -Wall

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

clean:
	$(RM) $(BIN)
