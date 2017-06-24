.PHONY: setup

setup:
	stack setup

.PHONY: build

build:
	stack build

.PHONY: dev

dev:
	stack test --fast --test-arguments "-j8 --hide-successes" --file-watch

.PHONY: test

test:
	stack test --fast --test-arguments "-j8"

.PHONY: runc

runc:
	stack exec massage-client

runs:
.PHONY: runs
	stack exec massage-server

run:
	stack exec 

.PHONY: clean

clean:
	stack clean

