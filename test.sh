
# TODO: test for broadcasts too
host=localhost:5055

function query() {
  curl --silent $@
}
function check() {
 if [[ "$1" == "$2" ]] ; then
     echo ok
 else
     echo error
 fi
}

query -X PUT $host/player/volume -d 10 > /dev/null
back=$(query localhost:5055/player/volume)

check $back 10

query -X PUT $host/player/volume -d 11 > /dev/null
back=$(query $host/player/volume)

check $back 11
