include bu.mk

.PHONY: doc

compile-erl:
	$(verbose) $(REBAR) compile

compile-ex: elixir
	$(verbose) $(MIX) deps.get
	$(verbose) $(MIX) compile

elixir:
	$(verbose) $(REBAR) elixir generate_mix
	$(verbose) $(REBAR) elixir generate_lib

tests:
	$(verbose) $(REBAR) eunit

lint:
	$(verbose) $(REBAR) lint

doc:
	$(verbose) $(REBAR) as doc edoc

dist: dist-ex dist-erl doc lint

release: dist-ex dist-erl
	$(verbose) $(REBAR) hex publish

dist-erl: distclean-erl compile-erl tests

distclean-erl: distclean
	$(verbose) $(RM_F) rebar.lock

dist-ex: distclean-ex compile-ex

distclean-ex: distclean
	$(verbose) $(RM_F) mix.lock

distclean:
	$(verbose) $(RM_RF) _build test/eunit deps ebin

