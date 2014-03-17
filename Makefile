DEPS = clojurian bitstring spiffy intarweb uri-common medea http-client fmt udp test uuid socket

# install for tradio:
# ci=aosp-chicken-install make

# default to host's chicken-install
ci ?= chicken-install
ciflags ?= -s -keep-installed

all: deps modules

modules: blobbery tone-generator dab dsp i2c restlib wimp
	$(ci) $(ciflags)

deps: socket
	$(ci) $(ciflags) $(DEPS)

blobbery:
	cd blobbery; $(ci) $(ciflags)

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

.PHONY: restlib i2c dsp biquad q523 dab tone-generator blobbery deps wimp all
