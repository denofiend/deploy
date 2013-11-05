.PHONY: rel deps

PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar

all:
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

rel:
	@$(REBAR) generate
clean:
	@$(REBAR) clean
clean_rel:
	@rm -rf rel/deploy

clean_all:
	@$(REBAR) clean
	@rm -rf rel/deploy

build_plt:
	@$(REBAR) build-plt

dialyzer:
	@$(REBAR) dialyze

