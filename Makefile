DEPS = clojurian bitstring spiffy intarweb uri-common \
	medea http-client fmt udp test uuid openssl \
	ssax sxpath sxml-serializer sql-de-lite

# install for tradio:
# ci=aosp-chicken-install make

# default to host's chicken-install
ci ?= chicken-install
ciflags ?= -s -keep-installed

all: deps modules

modules: blobbery looper pefat multicast tone-generator dab dsp i2c restlib wimp dlna
	$(ci) $(ciflags)

dlna:
	cd dlna ; $(ci) $(ciflags)

deps: 
	$(ci) $(ciflags) $(DEPS)

blobbery:
	cd blobbery; $(ci) $(ciflags)

looper:
	cd looper; $(ci) $(ciflags)

pefat:
	cd pefat; $(ci) $(ciflags)

multicast:
	cd multicast; $(ci) $(ciflags)

tone-generator:
	cd tone-generator; $(ci) $(ciflags)

dab:
	cd dab; $(ci) $(ciflags)

q523:
	cd dsp/q523; $(ci) $(ciflags)

biquad:
	cd dsp/biquad; $(ci) $(ciflags)

dsp: biquad q523
	cd dsp; $(ci) $(ciflags)

i2c:
	cd i2c; $(ci) $(ciflags)

restlib:
	cd restlib; $(ci) $(ciflags)

wimp:
	cd wimp; $(ci) $(ciflags)

# we patched up socket so it compiles with aosp-chicken-install.
socket:
	cd socket; $(ci) $(ciflags)

.PHONY: socket restlib i2c dsp biquad q523 dab tone-generator blobbery deps wimp all \
	multicast pefat dlna looper
