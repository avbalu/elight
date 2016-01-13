DEPS = $(CURDIR)/deps

DIALYZER_OPTS = -Wunderspecs

# List dependencies that should be included in a cached dialyzer PLT file.
DIALYZER_DEPS = deps/pkt/ebin \
                deps/epcap/ebin

DEPS_PLT = deps.plt

ERLANG_DIALYZER_APPS = asn1 \
                       compiler \
                       crypto \
                       edoc \
                       edoc \
                       erts \
		       et \
                       eunit \
                       eunit \
                       gs \
                       hipe \
                       inets \
                       kernel \
                       mnesia \
                       observer \
                       public_key \
                       runtime_tools \
                       runtime_tools \
                       ssl \
                       stdlib \
                       syntax_tools \
                       syntax_tools \
                       tools \
                       webtool \
		       wx \
                       xmerl

all: thrift compile eunit dialyzer

# Clean ebin and .eunit of this project
clean:
	@rebar clean skip_deps=true
	@rm -rf api/gen-erl

# Clean this project and all deps
allclean:
	@rebar clean

compile: $(DEPS)
	@rebar compile | fgrep -v -f ./compiler.ignore-warnings

$(DEPS):
	@rebar get-deps

# Full clean and removal of all deps. Remove deps first to avoid
# wasted effort of cleaning deps before nuking them.
distclean:
	@rm -rf deps $(DEPS_PLT)
	@rebar clean

eunit:
	@rebar skip_deps=true eunit | fgrep -v -f ./compiler.ignore-warnings | fgrep -v -f eunit.ignore


test: eunit

# Only include local PLT if we have deps that we are going to analyze
ifeq ($(strip $(DIALYZER_DEPS)),)
dialyzer: ~/.dialyzer_plt
	@dialyzer $(DIALYZER_OPTS) -r */ebin
else
dialyzer: ~/.dialyzer_plt $(DEPS_PLT)
	@dialyzer $(DIALYZER_OPTS) --plts ~/.dialyzer_plt $(DEPS_PLT) --no_check_plt -r */ebin | fgrep -v -f ./dialyzer.ignore-warnings

$(DEPS_PLT):
	@dialyzer --build_plt $(DIALYZER_DEPS) --output_plt $(DEPS_PLT)
endif

~/.dialyzer_plt:
	@echo "ERROR: Missing ~/.dialyzer_plt. Please wait while a new PLT is compiled."
	dialyzer --build_plt --apps $(ERLANG_DIALYZER_APPS)
	@echo "now try your build again"

doc:
	@rebar doc skip_deps=true

thrift:
#	@thrift --gen erl -o api api/thrift/types.thrift
#	@thrift --gen erl -o api api/thrift/config.thrift
#	@thrift --gen erl -o api api/thrift/status.thrift
	@thrift --gen erl -o api api/thrift/api.thrift


.PHONY: all compile eunit test dialyzer clean allclean distclean doc thrift
