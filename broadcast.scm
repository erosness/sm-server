;;; helpers for broadcasting alerts to peers.
;;;
;;; === glossary ===
;;; broadcast: any UDP multicase/broadcast message
;;;
;;; change-message: a HTTP-like packet describing a change in the
;;; statemap

(module broadcast (udp-broadcast change-message)

(import chicken scheme ports data-structures)
(use socket intarweb spiffy medea)

;; max packet length before the packet might be fragmented
;; note that this is not necessarily a problem, find out
;; http://stackoverflow.com/questions/14993000/the-most-reliable-and-efficient-udp-packet-size
(define max-udp-packet-size 1024)

;; send a UDP multicast containing msg and close.
(define (udp-broadcast msg #!optional (saddr (inet-address "239.255.255.250" 5055)))

  (and (> (string-length msg) max-udp-packet-size)
       (error "packet too large" (string-length msg)))

  (define s (socket af/inet sock/dgram 0))
  (set-socket-option s sol/socket so/broadcast 1)
  (socket-send-to s msg saddr)
  (socket-close s))

(define (udp-broadcast-headers rq)
  (let ((echo (header-value 'echo (request-headers rq))))
    (if echo
        (conc "\n" "Echo: " echo)
        "")))

;; create a message that represents `path` changing into `body`
(define (change-message path body)
  (conc "NOTIFY "
        path
        (udp-broadcast-headers (current-request))
        "\n\n" body))

;; like change-message, but used for large datasets. tbd
(define (make-alert path)
  (conc "ALERT "
        path
        (udp-broadcast-headers (current-request))
        "\n\n"))

(include "broadcast.tests.scm")
)
