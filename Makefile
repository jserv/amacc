CROSS_COMPILE =
CFLAGS = -Os -Wall -fsigned-char -g
LIBS = -ldl
BIN = amacc
SRC = $(BIN).c
QEMU = qemu-arm

ifeq ($(PREFIX), arm-linux-gnueabihf-)
	CROSS_COMPILE = arm-linux-gnueabihf
else
	CROSS_COMPILE = arm-linux-gnueabi
endif

CROSS_EXEC = $(QEMU) -L /usr/$(CROSS_COMPILE)

all: $(BIN)

amacc: amacc.c
	$(CROSS_COMPILE)-gcc $(CFLAGS) -o $(BIN) $(SRC) $(LIBS)

check: $(BIN)
	@echo "[ compiled ]"
	@$(CROSS_EXEC) ./$(BIN) tests/hello.c
	@echo "[ nested ]"
	@$(CROSS_EXEC) ./$(BIN) $(SRC) tests/hello.c
	@cloc --quiet $(SRC) 2>/dev/null

clean:
	$(RM) $(BIN)
