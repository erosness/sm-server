(module broadcast (udp-broadcast make-udp-message)

(import chicken scheme ports data-structures)
(use socket intarweb spiffy medea)

;; max packet length before the packet might be fragmented
;; note that this is not necessarily a problem, find out
;; http://stackoverflow.com/questions/14993000/the-most-reliable-and-efficient-udp-packet-size
(define max-udp-packet-size 508)

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

(define (make-notify path body)
  (conc "NOTIFY "
        path
        (udp-broadcast-headers (current-request))
        "\n\n" body))

(define (make-alert path)
  (conc "ALERT "
        path
        (udp-broadcast-headers (current-request))
        "\n\n"))

(define (make-udp-message path body)
  (let ((message (make-notify path body)))
    (if (< max-udp-packet-size (string-length message))
        (make-alert path)
        message)))

(include "broadcast.tests.scm")
)
