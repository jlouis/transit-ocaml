.PHONY: test
test: compile.byte
	./main.byte

.PHONY: compile
compile: compile.byte

compile.byte:
	corebuild -pkg yojson,oUnit -libs nums src/main.byte

compile:
	corebuild -pkg yojson,oUnit -libs nums src/main.native

.PHONY: clean
clean:
	rm -fr _build main.native
