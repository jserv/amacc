CFLAGS = -O0 -Wall -Wno-misleading-indentation
OBJ_DIR = elf
TEST_DIR = tests
TEST_SRC = $(wildcard $(TEST_DIR)/*.c)
TEST_OBJ = $(TEST_SRC:.c=.o)

BIN = amacc
PEEP = squint
EXEC = $(BIN) $(BIN)-native $(PEEP)

include mk/arm.mk
include mk/common.mk
include mk/python.mk

## Build AMaCC
all: $(EXEC)
$(BIN): $(BIN).c
	$(VECHO) "  CC+LD\t\t$@\n"
	$(Q)$(ARM_CC) $(CFLAGS) -o $@ $< -g -ldl

$(BIN)-native: $(BIN).c
	$(VECHO) "  CC+LD\t\t$@\n"
	$(Q)$(CC) $(CFLAGS) -o $@ $< \
	    -Wno-pointer-to-int-cast -Wno-int-to-pointer-cast -Wno-format \
	    -ldl

$(PEEP): $(PEEP).c
	$(VECHO) "  CC+LD\t\t$@\n"
	$(Q)$(ARM_CC) $(CFLAGS) -o $@ $< -g

## Run tests and show message
check: $(EXEC) $(TEST_OBJ)
	$(VECHO) "[ C to IR translation          ]"
	$(Q)./$(BIN)-native -s tests/arginc.c | diff tests/arginc.list - \
	    && $(call pass)
	$(VECHO) "[ JIT compilation + execution  ]"
	$(Q)if [ "$(shell $(ARM_EXEC) ./$(BIN) tests/hello.c)" = "hello, world" ]; then \
	$(call pass); \
	fi
	$(VECHO) "[ ELF generation               ]"
	$(Q)$(ARM_EXEC) ./$(BIN) -o $(OBJ_DIR)/hello tests/hello.c
	$(Q)if [ "$(shell $(ARM_EXEC) $(OBJ_DIR)/hello)" = "hello, world" ]; then \
	$(call pass); \
	fi
	$(VECHO) "[ nested/self compilation      ]"
	$(Q)if [ "$(shell $(ARM_EXEC) ./$(BIN) $(BIN).c tests/hello.c)" = "hello, world" ]; then \
	$(call pass); \
	fi
	$(VECHO) "[ Compatibility with GCC/Arm   ] "
	$(Q)$(PYTHON) scripts/runtest.py || echo

$(OBJ_DIR)/$(BIN): $(BIN)
	$(VECHO) "  SelfCC\t$@\n"
	$(Q)$(ARM_EXEC) ./$^ -o $@ $(BIN).c

$(OBJ_DIR)/$(BIN)-opt: $(BIN) $(PEEP)
	$(VECHO) "  SelfCC\t$@\n"
	$(Q)$(ARM_EXEC) ./$< -p -o $@ $(BIN).c
	$(Q)$(ARM_EXEC) scripts/peep $@

SHELL_HACK := $(shell mkdir -p $(OBJ_DIR))
$(TEST_DIR)/%.o: $(TEST_DIR)/%.c $(BIN) $(OBJ_DIR)/$(BIN) $(OBJ_DIR)/$(BIN)-opt
	$(VECHO) "[*** verify $< <JIT> *******]\n"
	$(Q)$(ARM_EXEC) ./$(BIN) $< 2 $(REDIR)
	$(VECHO) "[*** verify $< <ELF> *******]\n"
	$(Q)$(ARM_EXEC) ./$(BIN) -o $(OBJ_DIR)/$(notdir $(basename $<)) $< $(REDIR)
	$(Q)$(ARM_EXEC) $(OBJ_DIR)/$(notdir $(basename $<)) 2 $(REDIR)
	$(VECHO) "[*** verify $< <ELF-self> **]\n"
	$(Q)$(ARM_EXEC) ./$(OBJ_DIR)/$(BIN) $< 2 $(REDIR)
	$(Q)$(call pass,$<)

## Print available build targets
help:
	@cat $(MAKEFILE_LIST) | \
	 awk '/^##.*$$/{l1=$$0;getline;l2=(l1 "##" $$0); print l2 $$0}' | awk -F"##" '{split($$3,t,":");printf "\033[36m%-11s\033[0m %s\n",t[1],$$2}'

## Dump assembly from source file. Usage: "make dump-ir FILE=tests/hello.c"
dump-ir: $(BIN)
	@$(ARM_EXEC) $(BIN) -s $(FILE)

## Remove all generated files
clean:
	$(RM) $(EXEC) $(OBJ_DIR)/* elf/* out-gcc/*
