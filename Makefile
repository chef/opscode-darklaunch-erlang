DEPS = deps/json deps/meck deps/webmachine
#REBAR = `which rebar || echo ./rebar`
REBAR = ./rebar

all: compile

compile: $(DEPS)
	@$(REBAR) compile

clean:
	@$(REBAR) clean

distclean:
	@rm -rf deps ebin/* rel/darklaunch

$(DEPS):
	@$(REBAR) get-deps

test: eunit

eunit:
	@$(REBAR) skip_deps=true eunit

rel: compile
	@$(REBAR) generate

relclean:
	@rm -rf rel/darklaunch