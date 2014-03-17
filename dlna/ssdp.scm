(use socket http-client
     intarweb uri-common
     ssax)

;; ;; a version of socket-receive that never times out
;; (define (socket-receive* s size)
;;   (let loop ()
;;     (condition-case
;;      (socket-receive s 1024)
;;      ((exn i/o net timeout)
;;       (print "socket timeout, retrying")
;;       (loop)))))

;; much like udp-broadcast (which is a multicast) but doesn't close
;; socket and returns it
(define (udp-multicast msg #!optional (saddr (inet-address "239.255.255.250" 5055)))
  (define s (socket af/inet sock/dgram 0))
  (set-socket-option s sol/socket so/broadcast 1)
  (socket-send-to s msg saddr)
  s)

;; return a procedure which tells you if seconds has passed since
;; start-timer was evaluated. useful for timeouts.
(define (start-timer seconds)
  (let ((end (+ (current-milliseconds)
                (* seconds 1000))))
    (lambda () (> (current-milliseconds) end))))

(test-group
 "start-timer"
 (let ((timeout? (start-timer 0.05)))
   (test #f (timeout?))
   (thread-sleep! 0.1)
   (test #t (timeout?))))

;; ============================== actual search ==============================

;; create a query procedure which will return newly discovered UPnP
;; devices as constructed by fold (lambda (packet result) ...). fold is
;; called asynchronously in another thread.
;;
;; socket will be closed after timeout seconds.
(define (ssdp-search* timeout/sec fold initial)

  (define sock (udp-multicast
                "M-SEARCH * HTTP/1.1\r
HOST: 239.255.255.250:1900\r
MAN: \"ssdp:discover\"\r
MX: 5\r
ST: ssdp:all\r
\r
"
                (inet-address "239.255.255.250" 1900)))

  ;; folded result (so far)
  (define results initial)

  (define (update! packet)
    (set! results (fold packet results)))

  (define timeout? (start-timer timeout/sec))

  (define (thread-thunk)
    (let loop ()
      (condition-case
       (update! (socket-receive sock 1024))
       ;; ignore socket-receive timeouts
       [(exn i/o net timeout) (void)])
      (if (not (timeout?))
          (loop)))
    (socket-close sock))

  (thread-start! thread-thunk)

  (lambda () results))

;; you can use it like this:
;; (define results (ssdp-search* 30 cons '()))
;; wait 10-30seconds then eval (results)

;; ============================== helpers ==============================

;; add x to lst unless it's already there (using equal?)
(define (set-add x lst)
  (if (member x lst) lst
      (cons x lst)))

(test-group
 "set-add"
 (test '("a") (set-add "a" '()))
 (test '("a") (set-add "a" '("a")))
 (test '("c" "b" "a") (fold set-add '() '("a" "b" "c" "a"))))


;; extract LOCATION header value from packet as string.
(define (packet-location packet)
  (let ((headers (response-headers (read-response (open-input-string packet)))))
    (uri->string (header-value 'location headers))))

(test "packet-location"
      "http://host/path"
      (packet-location
       "HTTP/1.1 200 OK\r
LOCATION:http://host/path\r\n"))

;; do a search and make a list of location-uris only
;;
;; usage:
;;
;; (define results (ssdp-search 30))
;; wait couple of seconds
;; (results)
(define (ssdp-search timeout/sec)
  (ssdp-search*
   timeout/sec
   (lambda (x lst) (set-add (packet-location x) lst))
   '()))

;; ==================== multicast ====================
;; incoming multicast from my MiniDLNA when it starts:
"NOTIFY * HTTP/1.1
Cache-control: max-age=1800
Host: 239.255.255.250:1900
Usn: uuid:1ea984fa-617f-e852-ffff-ffffbf34e89a::urn:schemas-upnp-org:device:MediaServer:1
Location: http://10.0.0.29:58645/dev/1ea984fa-617f-e852-ffff-ffffbf34e89a/desc.xml
Nt: urn:schemas-upnp-org:device:MediaServer:1
Nts: ssdp:alive
Server: Linux/3.4.0-perf-g2cae413 UPnP/1.0 BubbleUPnP/1.8.2
"
