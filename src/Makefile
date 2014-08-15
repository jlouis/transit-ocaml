.DEFAULT_GOAL := compile

build:
	corebuild transit.cma
	corebuild transit.cmxa

install:
	ocamlfind install transit META _build/*

remove:
	ocamlfind remove transit

clean:
	corebuild -clean
