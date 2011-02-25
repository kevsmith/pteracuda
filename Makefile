all: compile test

compile:
	@./rebar compile

clean:
	@./rebar clean
	@rm -f c_src/*.o c_src/*flymake.h

test:
	@./rebar eunit

eunit:
	@./rebar eunit
