REBAR=		./rebar
DIALYZER=	dialyzer


.PHONY: all deps compile get-deps clean

all: compile release

deps: clean get-deps

package: clean get-deps compile release bundle

get-deps:
	@$(REBAR) get-deps

compile: compile-erl 

compile-erl:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

repl:
	erl -pz `pwd`/deps/*/ebin -pa `pwd`/ebin -s fingwb 

release: compile
	@$(REBAR) generate

bundle: release
	mkdir -p packages;
	tar czf packages/fingwb.tar.gz -C rel fingwb
