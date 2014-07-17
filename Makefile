REBAR := ./rebar
REBAR_URL := https://github.com/rebar/rebar/wiki/rebar
ERL       ?= erl

.PHONY: compile test

all: script

script: compile
	$(REBAR) escriptize

compile: $(REBAR)
	$(REBAR) get-deps compile

clean: $(REBAR)
	$(REBAR) clean
	rm -f pkgx

./rebar:
	$(ERL) -noshell -s inets -s ssl \
	  -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
	  -s init stop
	chmod +x ./rebar
