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

munge_apps:
	@mkdir -p rel/apps/darklaunch
	@cp -R ebin rel/apps/darklaunch/ebin
	@cp -R priv rel/apps/darklaunch/priv

rel: compile munge_apps
	@$(REBAR) generate
	@rm -rf rel/apps

relclean:
	@rm -rf rel/darklaunch
	@rm -rf rel/apps

update:
	@cd rel/darklaunch;bin/darklaunch restart

devrel: rel
	@/bin/echo -n Symlinking deps and apps into release
	@$(foreach dep,$(wildcard deps/*), /bin/echo -n .;rm -rf rel/darklaunch/lib/$(shell basename $(dep))-* \
           && ln -sf $(abspath $(dep)) rel/darklaunch/lib;)
	@rm -rf rel/darklaunch/lib/darklaunch-*;mkdir -p rel/darklaunch/lib/darklaunch
	@ln -sf `pwd`/ebin rel/darklaunch/lib/darklaunch
	@ln -sf `pwd`/priv rel/darklaunch/lib/darklaunch
	@/bin/echo done.
	@/bin/echo  Run \'make update\' to pick up changes in a running VM.
