.PHONY:	all deps check test clean

all: deps
	./rebar compile

deps:
	./rebar get-deps

docs:
	./rebar doc

check:
	./rebar check-plt
	./rebar dialyze

test:
	./rebar eunit


clean:
	./rebar clean
	$(RM) doc/*
	$(RM) ebin/*.d

