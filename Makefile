CWD=$(shell pwd)
NAME=$(shell basename ${CWD})

all: conf clean compile
	./rebar compile

clean:
	./rebar clean
	rm -rf rel/${NAME}

compile: 
	./rebar compile

$(MK_INCLUDE): $(MK_INCLUDE).in
	$(MAKE) conf

conf:
	(cd config; $(MAKE))

conf_clean:
	(cd config; $(MAKE) clean)

config/configure: config/configure.in
	(cd config; autoconf)

