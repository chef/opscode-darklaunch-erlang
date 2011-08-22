DEPS = deps/json deps/meck
#REBAR = `which rebar || echo ./rebar`
REBAR = ./rebar

all: compile

compile: $(DEPS)
	@$(REBAR) compile

clean:
	@$(REBAR) clean

distclean:
	@rm -rf deps ebin/*

$(DEPS):
	@$(REBAR) get-deps

test: eunit

eunit:
	@$(REBAR) skip_deps=true eunit