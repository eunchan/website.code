.PHONY: all

all: build install

build:
	stack build

install:
	stack install
