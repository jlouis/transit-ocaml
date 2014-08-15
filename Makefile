.PHONY: build
build:
	$(MAKE) -C src build
	
.PHONY: install
install:
	$(MAKE) -C src install
	
.PHONY: remove
remove:
	$(MAKE) -C src remove

.PHONY: test
test:
	$(MAKE) -C test compile.byte
	./test/main.byte
