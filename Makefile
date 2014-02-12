DEPS = clojurian bitstring spiffy intarweb uri-common medea http-client fmt socket udp

all: blobbery tone-generator dab dsp i2c restlib wimp
	chicken-install -s

deps:
	chicken-install -s $(DEPS)

blobbery:
	cd blobbery; chicken-install -s

tone-generator:
	cd tone-generator; chicken-install -s

dab:
	cd dab; chicken-install -s

q523:
	cd dsp/q523; chicken-install -s

biquad:
	cd dsp/biquad; chicken-install -s

dsp: biquad q523
	cd dsp; chicken-install -s

i2c:
	cd i2c; chicken-install -s

restlib:
	cd restlib; chicken-install -s

wimp:
	cd wimp; chicken-install -s


.PHONY: restlib i2c dsp biquad q523 dab tone-generator blobbery deps wimp all
