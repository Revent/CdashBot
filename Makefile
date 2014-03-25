ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/apps/*/ebin -pa $(CURDIR)/deps/*/ebin

# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
ERL = $(shell which erl)

ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

REBAR=./rebar

ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

.PHONY: all compile  clean distclean deps update-deps compile rebuild

all: deps compile 

# =============================================================================
# Rules to build the system
# =============================================================================

deps:
	$(REBAR) get-deps
	$(REBAR) compile

update-deps:
	$(REBAR) update-deps
	$(REBAR) compile

compile:
	$(REBAR) skip_deps=true compile
	$(REBAR) generate

clean:
	$(REBAR) skip_deps=true clean

distclean: clean
	- rm -rvf $(CURDIR)/deps

rebuild: distclean deps compile 
