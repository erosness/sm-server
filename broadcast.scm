(use socket)

(define (udp-broadcast msg #!optional (saddr (inet-address "239.255.255.250" 5055)))
  (define s (socket af/inet sock/dgram 0))
  (set-socket-option s sol/socket so/broadcast 1)
  (socket-send-to s msg saddr)
  (socket-close s))

(define (udp-broadcast-headers rq)
  (let ((echo (header-value 'echo (request-headers rq))))
    (if echo
        (conc "\n" "Echo: " echo)
        "")))

