HAS_ELIXIR=1

include bu.mk

release: $(DIST)
	$(verbose) $(REBAR) hex publish

