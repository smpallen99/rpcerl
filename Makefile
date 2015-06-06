all: conf
	(cd src; $(MAKE) $@)

clean:
	(cd src; $(MAKE) $@)

$(MK_INCLUDE): $(MK_INCLUDE).in
	$(MAKE) conf

conf:
	(cd config; $(MAKE))

conf_clean:
	(cd config; $(MAKE) clean)

config/configure: config/configure.in
	(cd config; autoconf)

