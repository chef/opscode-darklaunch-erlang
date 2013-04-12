DEPS = deps
REBAR = rebar

all: compile test dialyze

compile: $(DEPS)
	@$(REBAR) compile

dialyze: compile
	@dialyzer -Wunderspecs -r ebin

clean:
	@$(REBAR) clean

distclean: clean
	@rm -rf deps ebin/* rel/darklaunch

$(DEPS):
	@$(REBAR) get-deps

test: eunit

eunit: compile
	@$(REBAR) skip_deps=true eunit

munge_apps:
	@mkdir -p rel/apps/darklaunch
	@ln -sf `pwd`/ebin rel/apps/darklaunch
	@ln -sf `pwd`/priv rel/apps/darklaunch
	@cp rebar.config rel
	@echo '{deps_dir, ["../deps"]}.' >> rel/rebar.config

generate: munge_apps
	@cd rel;$(REBAR) generate
	@rm -rf rel/apps rel/rebar.config
	@echo '___  ____ ____ _  _ _    ____ _  _ _  _ ____ _  _ '
	@echo '|  \ |__| |__/ |_/  |    |__| |  | |\ | |    |__| '
	@echo '|__/ |  | |  \ | \_ |___ |  | |__| | \| |___ |  | '
	@echo ''

rel: compile munge_apps generate

# Use this to build Darklaunch in a cookbook... no need to dialyze
# in that environment
prodrel: compile munge_apps generate

relclean:
	@rm -rf rel/darklaunch
	@rm -rf rel/apps

update: compile
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
