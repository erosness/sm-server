
DEPS = \
 uri-generic:2.43 \
 args:1.4.4 \
 message-digest:3.4.0 \
 md5:3.2.0 \
 bitstring:1.33 \
 clojurian:0.0.1 \
 fmt:0.805 \
 gochan:0.3 \
 http-client:0.7.1 \
 intarweb:1.3 \
 matchable:3.3 \
 medea:2 \
 nanomsg:0.3 \
 nrepl:0.1 \
 openssl:1.6.4 \
 spiffy:5.3.2 \
 sql-de-lite:0.6.6 \
 ssax:5.0.7 \
 sxml-serializer:0.4 \
 sxpath:0.2 \
 test:0.9.9.6 \
 udp:1.18 \
 uri-common:1.4 \
 uuid:0.1 \
 spiffy-cgi-handlers:0.5 \


# install for tradio:
# ci=aosp-chicken-install make

# default to host's chicken-install
ci ?= chicken-install
ciflags ?= -s -keep-installed

all: deps modules

modules: blobbery looper pefat multicast tone-generator dab i2c restlib wimp tunein dlna
	$(ci) $(ciflags)

dlna:
	cd dlna ; $(ci) $(ciflags)

deps:
	$(ci) $(ciflags) $(DEPS)

blobbery:
	cd blobbery; $(ci) $(ciflags)

looper: socket
	cd looper; $(ci) $(ciflags)

pefat:
	cd pefat; $(ci) $(ciflags)

multicast:
	cd multicast; $(ci) $(ciflags)

tone-generator:
	cd tone-generator; $(ci) $(ciflags)

dab: i2c
	cd dab; $(ci) $(ciflags)

i2c:
	cd i2c; $(ci) $(ciflags)

restlib:
	cd restlib; $(ci) $(ciflags)

wimp:
	cd wimp; $(ci) $(ciflags)

tunein:
	cd tunein; $(ci) $(ciflags)

nics:
	cd nics; $(ci) $(ciflags)

# we patched up socket so it compiles with aosp-chicken-install.
socket:
# you can inspect env variables with this:
#	$(error	"what to do" $(shell env))
# but make sure you uninstall socket.so from out/host/
ifdef ANDROID_BUILD_TOP
# building for aosp
	cd socket; $(ci) $(ciflags)
else
# building for dev-machine
	$(ci) $(ciflags) socket
endif


.PHONY: socket restlib i2c dab tone-generator blobbery deps wimp all \
	multicast pefat dlna looper nics tunein
