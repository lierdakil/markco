MY_FILES=$(shell git ls-files .)

.PHONY: all
all: dist

dist: $(MY_FILES)
	rm -r dist/
	npm install
	npm install --only=dev
	npm run gulp
