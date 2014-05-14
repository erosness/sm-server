#!/bin/sh
#| -*- scheme -*-
exec csi -s "$0" "$@"
|#

(use multicast srfi-18 socket looper clojurian-syntax)

(define s (multicast-listen-socket 5055))

((->> (lambda ()
        (receive (packet addr) (socket-receive-from s 2048)
          (pp (list (current-milliseconds)
                    packet
                    (sockaddr-address addr)))))
      (loop/socket-timeout)
      (loop)
      (with-socket s)))

