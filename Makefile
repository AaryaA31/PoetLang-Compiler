
BUILD_CMD = ocamlbuild -use-ocamlfind -pkgs llvm PoetLang.native

MC_SRCS := $(wildcard *example.mc)
OUTS    := $(MC_SRCS:.mc=.out)
BINARY  := poetlang.native

.PHONY: all build test clean

all: build test

build:
	$(BUILD_CMD)

test: build $(OUTS)
	@echo "== Running LLVM IR =="
	@for out in $(OUTS); do \
	  printf "\n-- $$out --\n"; \
	  lli $$out; \
	done

%.out: %.mc build
	./$(BINARY) -l $< > $@

clean:
	ocamlbuild -clean
	rm -f $(OUTS)
