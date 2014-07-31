compile.byte:
	corebuild -pkg yojson,oUnit -libs nums src/main.byte

compile:
	corebuild -pkg yojson,oUnit -libs nums src/main.native

clean:
	rm -fr _build main.native
