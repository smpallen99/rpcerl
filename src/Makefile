# Copyright (c) 2000, 2001 Sendmail, Inc.  All rights reserved.
# This Makefile may require GNU make.

include ../support/include.mk
include ../vsn.mk

ERLC_FLAGS += -I ../..

ESRC=.
ERPCGEN = ../priv/erpcgen
ERPCGEN_OPTS =

ERPCGEN_MODULES = erpcgen xdr_parse xdrgen xdr_scan

RPC_MODULES = rpc_client rpc_xdr rpc_server \
	            pmap pmap_clnt pmap_xdr \
	            xdr_auth 

MODULES =  $(RPC_MODULES) $(ERPCGEN_MODULES)

ERPCGEN_TARGET_FILES= $(ERPCGEN_MODULES:%=../ebin/%.$(EMULATOR))
RPC_TARGET_FILES= $(RPC_MODULES:%=../ebin/%.$(EMULATOR))
TARGET_FILES= $(MODULES:%=../ebin/%.$(EMULATOR)) $(ERPCGEN)

%_xdr.erl %.hrl: %.x $(ERPCGEN)
	$(ERPCGEN) -a '[xdrlib]' $*

%_clnt.erl: %.x $(ERPCGEN)
	$(ERPCGEN) -a '[clnt]' $*

%_svc.erl: %.x $(ERPCGEN)
	$(ERPCGEN) -a '[svc]' $*

%_svc_callback.erl: %.x $(ERPCGEN)
	$(ERPCGEN) -a '[svc_callback]' $*

$(EBIN)/%.$(EMULATOR): $(ESRC)/%.erl
	$(ERLC) $(ERLC_FLAGS) -o$(EBIN) $<

all: $(TARGET_FILES)

$(ERPCGEN): erpcgen.src $(ERPCGEN_TARGET_FILES)
	sed -e 's;%ERL%;$(ERL);' \
	  erpcgen.src > $(ERPCGEN)
	chmod u+x $(ERPCGEN)

xdr_parse.erl: xdr.yrl
	$(ERLC) -o xdr_parse.erl xdr.yrl

clean:
	-rm -f $(TARGET_FILES)
	-rm -f core erl_crash.dump
	-rm -f rpc_xdr.erl 
	-rm -f pmap_clnt.erl pmap_xdr.erl pmap.hrl
	-rm -f erpcgen.boot erpcgen.script xdr_parse.erl
	-rm -f ../ebin/erpcgen.app ../ebin/rpc.app erpcgen.rel

../ebin/rpc_client.beam: rpc.hrl
../ebin/rpc_server.beam: rpc.hrl
../ebin/pmap.beam: pmap.hrl
../ebin/pmap_clnt.beam: pmap.hrl
../ebin/pmap_svc.beam: pmap.hrl
rpc_xdr.erl: rpc.
pmap_xdr.erl: pmap.x
pmap_clnt.erl: pmap.x

