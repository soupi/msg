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

.PHONY: runclient

runclient:
	stack exec msg-client

.PHONY: runserver

runserver:
	stack exec msg-server

.PHONY: clean

clean:
	stack clean

