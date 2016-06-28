.PHONY: doc
REBAR = ./rebar3
MIX = mix

compile-erl:
	@$(REBAR) compile

compile-ex: elixir
	@$(MIX) deps.get
	@$(MIX) compile

elixir:
	@$(REBAR) elixir generate_mix
	@$(REBAR) elixir generate_lib

tests:
	@$(REBAR) eunit

doc:
	@$(REBAR) as doc edoc

dist: dist-ex dist-erl doc

release: dist-ex dist-erl
	@$(REBAR) hex publish

dist-erl: distclean-erl compile-erl tests

distclean-erl: distclean
	@rm -f rebar.lock

dist-ex: distclean-ex compile-ex

distclean-ex: distclean
	@rm -f mix.lock

distclean:
	@rm -rf _build test/eunit deps rebar.lock mix.lock

