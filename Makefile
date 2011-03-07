all: compile eunit

compile:
	@./rebar compile

clean:
	@./rebar clean
	@rm -f c_src/*.o c_src/*flymake.h

eunit:
	@./rebar eunit
