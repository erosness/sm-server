(use socket intarweb medea)

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

(define (make-notify path json)
  (conc "NOTIFY "
        path
        (udp-broadcast-headers (current-request))
        "\n\n" json))

(define ((bc proc path) #!rest args)
  (let* ((response (apply proc args))
         (json (with-output-to-string (lambda () (write-json response)))))
    (udp-broadcast (make-notify path json))
    response))

