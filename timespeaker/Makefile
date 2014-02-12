
file:=counter.mp3

all: $(file)

$(file):
	time csi -s timespeaker.scm | ffmpeg -ar 22050 -f u8 -i - $(file)	
	echo made $(file)
