REBAR := ./rebar3
REBAR_URL := https://s3.amazonaws.com/rebar3/rebar3
ERL       ?= erl

.PHONY: compile test

all: script

script: compile
	$(REBAR) escriptize
	cp _build/default/bin/pkgx .

compile: $(REBAR)
	$(REBAR) compile

clean: $(REBAR)
	$(REBAR) clean
	rm -f pkgx

$(REBAR):
	$(ERL) -noshell -s inets -s ssl \
	  -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "$(REBAR)"}])' \
	  -s init stop
	chmod +x $(REBAR)
