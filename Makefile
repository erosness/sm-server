DEPS = clojurian bitstring spiffy intarweb uri-common \
	medea http-client fmt udp test uuid openssl \
	ssax sxpath sxml-serializer sql-de-lite nrepl \
	matchable args gochan

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
