(use socket http-client
     intarweb uri-common
     multicast
     ssax test looper)

;; ;; a version of socket-receive that never times out
;; (define (socket-receive* s size)
;;   (let loop ()
;;     (condition-case
;;      (socket-receive s 1024)
;;      ((exn i/o net timeout)
;;       (print "socket timeout, retrying")
;;       (loop)))))


;; returns the UDP packet body according to ssdp search query message
;; format.
(define (ssdp-search-message adr)
  (conc "M-SEARCH * HTTP/1.1\r
HOST: " (sockaddr-address adr) ":" (sockaddr-port adr) "\r
MAN: \"ssdp:discover\"\r
MX: 5\r
ST: ssdp:all\r
\r
"))

(test "ssdp-search-message"
      "M-SEARCH * HTTP/1.1\r
HOST: 10.0.0.1:1234\r
MAN: \"ssdp:discover\"\r
MX: 5\r
ST: ssdp:all\r
\r
"
      (ssdp-search-message (inet-address "10.0.0.1" 1234)))

;; ============================== actual search ==============================

;; create a query procedure which will return newly discovered UPnP
;; devices as constructed by fold (lambda (packet result) ...). fold is
;; called asynchronously in another thread.
;;
;; socket will be closed after timeout seconds.
(define (ssdp-search* timeout/sec fold initial
                      #!optional
                      (address (inet-address "239.255.255.250" 1900))
                      (msg (ssdp-search-message address)))

  (define sock (udp-multicast* msg address))

  ;; folded result (so far)
  (define results initial)

  (define (update!)
    (let-values (((packet remote) (socket-receive-from sock 1024)))
      (set! results (fold packet remote results))))

  (thread-start! (->> update!
                      (loop/socket-timeout)
                      (loop/timeout timeout/sec)
                      (loop)
                      (with-socket sock)))

  (lambda () results))

(test-group
 "ssdp-search*"
 ;; this is a tricky one. we wanna make sure that timeout is respected
 ;; and socket-close is called in the end.
 (let ((closed? #f))
   (fluid-let ((udp-multicast (lambda (m a) #f))
               (socket-close (lambda (s) (set! closed? #t)))
               (socket-receive (lambda (s l) (thread-sleep! 0.02) "packet")))
     (let ((proc (ssdp-search* 0.01 (lambda (x a r) (cons x r)) '())))
       (thread-sleep! 0.2)
       (test "receiving one packet only" '("packet") (proc))
       (test "thread terminates and socket is closed" #t closed?)))))

;; you can use it like this:
;; (define results (ssdp-search* 30 list '()))
;; wait 10-30seconds then eval (results)

;; ============================== helpers ==============================

;; extract LOCATION header value from packet as string.
(define (packet-location packet)
  (let ((headers (response-headers (read-response (open-input-string packet)))))
    (uri->string (header-value 'location headers))))

(test "packet-location"
      "http://host/path"
      (packet-location
       "HTTP/1.1 200 OK\r
LOCATION:http://host/path\r\n"))


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
