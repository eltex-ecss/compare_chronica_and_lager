.PHONY: deps compile release start

all: clean deps compile release start

deps:
	rebar get-deps

compile:
	rebar compile

release:
	relx

start:
	./release/sella/bin/sella-0.0.1 console -noshell

clean:
	rebar delete-deps
	-rm -rf ebin ./release/sella