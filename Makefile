CROSS_COMPILE = arm-linux-gnueabihf-
CFLAGS = -Os -Wall

BIN = amacc
all: $(BIN)

amacc: amacc.c
	$(CROSS_COMPILE)gcc $(CFLAGS) -fsigned-char -o amacc amacc.c -g -ldl

check: $(BIN)
	@echo "[ compiled ]"
	@qemu-arm -L /usr/arm-linux-gnueabihf ./amacc hello.c
	@echo "[ nested ]"
	@qemu-arm -L /usr/arm-linux-gnueabihf ./amacc amacc.c hello.c
	@cloc --quiet amacc.c 2>/dev/null

clean:
	$(RM) $(BIN)
