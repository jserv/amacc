all: vm

vm: vm.c fstring.c
	$(CC) $(CFLAGS) -Wall -Wextra -g -o $@ $^ -ldl

clean:
	$(RM) *.o vm
