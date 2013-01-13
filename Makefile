REL_FILES_DIR?=rel/files

ERL?=erl
REBAR?=./rebar
ERL_OPTS+=-pa ebin -pa deps/*/ebin -bool start_sasl -config $(REL_FILES_DIR)/sys.config

all: compile

compile: deps
	$(REBAR) compile

doc:
	$(REBAR) doc

node: generate
	rel/qerl/bin/qerl console

start:
	rel/qerl/bin/qerl console

start_debug: compile
	$(ERL) $(ERL_OPTS) -s qerl -eval 'debugger:start().'

test:
	$(REBAR) compile
	$(REBAR) skip_deps=true eunit

# Run all tests (including testing the deps)
test_all:
	$(REBAR) eunit

generate: compile
	$(REBAR) generate -f

release: generate
	tar -czf qerl.tar.gz rel/qerl

dialyzer:
	dialyzer -q -n ebin -Wunmatched_returns -Werror_handling \
		-Wrace_conditions > dialyzer_warnings

.PHONY: deps
deps:
	mkdir deps || true
	$(REBAR) get-deps

clean:
	$(REBAR) clean
