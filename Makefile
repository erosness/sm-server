DEPS = clojurian bitstring spiffy intarweb uri-common medea http-client fmt udp test

# install for tradio:
# ci=aosp-chicken-install

ci=chicken-install

all: deps modules

modules: blobbery tone-generator dab dsp i2c restlib wimp
	$(ci) -s

deps: socket
	$(ci) -s $(DEPS)

blobbery:
	cd blobbery; $(ci) -s

tone-generator:
	cd tone-generator; $(ci) -s

dab:
	cd dab; $(ci) -s

q523:
	cd dsp/q523; $(ci) -s

biquad:
	cd dsp/biquad; $(ci) -s

dsp: biquad q523
	cd dsp; $(ci) -s

i2c:
	cd i2c; $(ci) -s

restlib:
	cd restlib; $(ci) -s

wimp:
	cd wimp; $(ci) -s

# we patched up socket so it compiles with aosp-chicken-install.
socket:
	cd socket; $(ci) -s

.PHONY: restlib i2c dsp biquad q523 dab tone-generator blobbery deps wimp all
