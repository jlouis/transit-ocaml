compile.byte:
	corebuild -pkg yojson -libs nums src/main.byte

compile:
	corebuild -pkg yojson -libs nums src/main.native

clean:
	rm -fr _build main.native
