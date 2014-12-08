(module multicast ( ssdp-multicast-group
                    ssdp-port udp-multicast* udp-multicast
                    multicast-listen-socket )

(import chicken scheme)
(use socket udp)

(define ssdp-multicast-group "239.255.255.250")
(define ssdp-port 1900)

;; max packet length before the packet might be fragmented
;; note that this is not necessarily a problem, find out
;; http://stackoverflow.com/questions/14993000/the-most-reliable-and-efficient-udp-packet-size
(define max-udp-packet-size 1024)


;; send a UDP multicast message and return its socket
(define (udp-multicast* msg saddr)

  (and (> (string-length msg) max-udp-packet-size)
       (error "packet too large" (string-length msg)))

  (define s (socket af/inet sock/dgram 0))
  (set-socket-option s sol/socket so/broadcast 1)
  ;; socket-send-to may error out (if you're offline for
  ;; example). don't leak sockets in this case.
  (condition-case (socket-send-to s msg saddr)
                  (e (exn i/o net)
                     (socket-close s)
                     (signal e)))
  s)

;; send a UDP multicast message and close its socket
(define (udp-multicast msg #!optional (saddr (inet-address ssdp-multicast-group 5055)))
  (socket-close (udp-multicast* msg saddr))
  (void))

;; create a UDP socket that listsens for multicast messages
(define (multicast-listen-socket #!optional (port ssdp-port) (multicast-group ssdp-multicast-group))
  (define s (socket af/inet sock/dgram))
  (set! (so-reuse-address? s) #t)
  (udp-bind! s multicast-group port)
  (udp-join-multicast-group s #f multicast-group #t)
  s)


(use test)
(test-group
 "udp-multicast"
 (test (void) (udp-multicast "unit-test: hi"))
 (test-error (udp-multicast (make-string 2048 #\a))))

)
