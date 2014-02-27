(use socket udp)

;; (include "broadcast.scm")

;; (define port 5055)
;; TODO: proper logging
(define *prefix* "> ")

(define heartbeat-packet "HEARTBEAT /device\n\nHello friends")
(define (heartbeat intervall)
  (let loop ()
    (udp-broadcast heartbeat-packet)
    (print "Heartbeat")
    (thread-sleep! intervall)
    (loop)))

;; (define t (thread-start! (lambda () (heartbeat 1))))
;; (thread-terminate! t)

(define (make-multicast-listen-socket multicast-group port)
  (define s (socket af/inet sock/dgram))
  (set! (so-reuse-address? s) #t)
  (udp-bind! s multicast-group port)
  (udp-join-multicast-group s #f multicast-group #t)
  s)

;; (define port 5055)
(define (start-discovery port heartbeat-interval)
  (thread-start! (lambda () (heartbeat heartbeat-interval)))
  (define multicast-group "239.255.255.250")
  (print *prefix* "Listening for search requests on " multicast-group ":" port "...")
  (define s (make-multicast-listen-socket "239.255.255.250" port))
  (let loop ()
    (if (socket-receive-ready? s)
        (let-values [((msg addr) (socket-receive-from s 256))]
          (if (string-ci= "SEARCH" msg)
              (let ((reply-socket (socket af/inet sock/dgram))
                    (host (sockaddr-address addr)))
                (print *prefix* "Sending answer to " host ":" port)
                (socket-send-to reply-socket heartbeat-packet
                                (inet-address host port)))))
        (thread-sleep! 0.005))
    (loop)))

;; (start-discovery 5055 360)
;; (socket-close s)
;; (thread-start! (lambda () (start-discovery 5055 360)))
