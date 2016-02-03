CROSS_COMPILE =
CFLAGS = -Os -Wall
LIBS = -ldl
BIN = amacc
QEMU = qemu-arm

ifeq ($(PREFIX), arm-linux-gnueabihf-)
	CROSS_COMPILE = arm-linux-gnueabihf
else
	CROSS_COMPILE = arm-linux-gnueabi
endif

CROSS_EXEC = $(QEMU) -L /usr/$(CROSS_COMPILE)

all: $(BIN)

amacc: amacc.c
	$(CROSS_COMPILE)-gcc $(CFLAGS) -fsigned-char -o amacc amacc.c -g $(LIBS)

check: $(BIN)
	@echo "[ compiled ]"
	@$(CROSS_EXEC) ./amacc tests/hello.c
	@echo "[ nested ]"
	@$(CROSS_EXEC) ./amacc amacc.c tests/hello.c
	@cloc --quiet amacc.c 2>/dev/null

clean:
	$(RM) $(BIN)
