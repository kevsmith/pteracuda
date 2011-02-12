all: compile test

compile:
	@./rebar compile

clean:
	@./rebar clean

test:
	@./rebar eunit

eunit:
	@./rebar eunit
