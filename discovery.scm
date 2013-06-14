(use socket udp)

;; (define port 5055)
;; TODO: proper logging
(define *prefix* "> ")

(define (make-multicast-listen-socket multicast-group port)
  (define s (socket af/inet sock/dgram))
  (set! (so-reuse-address? s) #t)
  (udp-bind! s multicast-group port)
  (udp-join-multicast-group s #f multicast-group #t)
  s)


(define (start-discovery port)
  (define multicast-group "239.255.255.250")
  (print *prefix* "Listening for search requests on " multicast-group ":" port "...")
  (define s (make-multicast-listen-socket "239.255.255.250" port))
  (let loop ()
      (let-values [((msg addr) (socket-receive-from s 256))]
        (if (string-ci= "SEARCH" msg)
            (let ((reply-socket (socket af/inet sock/dgram))
                  (host (sockaddr-address addr)))
              (print *prefix* "Sending answer to " host ":" port)
              (socket-send-to reply-socket "NOTIFY /device\n\nADD VERSION HERE"
                              (inet-address host port))))
        )
      loop
      (loop)))
