
# Cube REST-Server

The central brain, pulling all strings. It exposes a REST-API and
triggers UDP notifications for any state-changes. It's main tasks are:

- audio source search (e.g. searching for Queen)
- playqueue manipulation
- audio metadata manipulation (eg. volume)
- audio playback manipulation (eg. pause)

# json

All requests and responses should be strict JSON. That is, everything
is either an array or an object. Hmmm ... kinda silly for things like

    curl -X PUT localhost:5055/player/volume -d 75

which would become something like

    curl -X PUT localhost:5055/player/volume -d '{ "val": 75}'


# turi

A unique identifier which allows the cube-server to address an audio
track/stream. For example:

  tr://tone/440
  tr://wimp/tid/12345
  file:///media/disk/music/song.mp3

Search modules in cube-server typically generate turis which their
corresponding play-modules can parse and play. A tr://wimp/tid/1234 can
generate a http stream-url using the Wimp API, for example.

Poke around for ffmpeg-command, wimp-command and tone-command.

# playqueue / pq

This is an non-persistent list of turis which are about to be played.
It is a central piece of the player. Each zone has its own playqueue.
When users control their player, they are really manipulating this
playqueue.

Pressing "next song" stops the current playback, select the next track
on the pq and restarts playback on that song. The old track is still
in the pq. There is a pq cursor!


* Todo Cube-Server
** Build
*** TODO Build everything with one `make`
*** TODO fix socket egg problem
*** TODO keep versioning of deps under control
** cube-server
*** TODO make start without network interfaces robust
*** modulerize
**** zones may come and go
**** ease development process (eg add search engine for testing)
*** DONE wrap all errors in HTTP response (see wrap-errors)

