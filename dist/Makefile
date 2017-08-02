SERVER_FILES=$(shell git ls-files ../server)

.PHONY: all ../client
all: markco server

markco: ../client
	rm -r markco/ || true
	cp -r ../client/dist/ markco/

server: $(SERVER_FILES)
	rm -r server/ || true
	mkdir server/
	git ls-files ../server/ | cpio -pdm server/

../client:
	make -C ../client
