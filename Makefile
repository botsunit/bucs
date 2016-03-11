PROJECT = bucs

DEP_PLUGINS = mix.mk
BUILD_DEPS = mix.mk
ELIXIR_VERSION = ~> 1.2
ELIXIR_BINDINGS = bucs bucbinary buccode bucfile bucinet buclists bucmaps bucmime bucos bucrandom bucstring buctimer bucuri bucdate

dep_mix.mk = git https://github.com/botsunit/mix.mk.git master

DOC_DEPS = edown
dep_edown = git https://github.com/botsunit/edown.git master

include erlang.mk

CFLAGS += -D_BSD_SOURCE

EDOC_OPTS = {doclet, edown_doclet} \
						, {app_default, "http://www.erlang.org/doc/man"} \
						, {source_path, ["src"]} \
						, {overview, "overview.edoc"} \
						, {stylesheet, ""} \
						, {image, ""} \
						, {top_level_readme, {"./README.md", "https://github.com/botsunit/${PROJECT}"}}

EUNIT_OPTS = verbose, {report, {eunit_surefire, [{dir, "test"}]}}

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include

release: app mix.all
