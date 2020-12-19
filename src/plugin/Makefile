.POSIX:

CC = cc
CFLAGS = -Wall -Wextra -pedantic
DUNE = dune

plugin.so: signal_weechat_ocaml.ml description.c
	$(DUNE) build ./signal_weechat_ocaml.exe.o
	$(CC) $(CFLAGS) -o $@ -shared \
		$$($(DUNE) exec ocamlfind query weechat)/plugin/plugin_boilerplate.o \
		../../_build/default/src/plugin/signal_weechat_ocaml.exe.o

clean:
	rm -f *.o *.so
