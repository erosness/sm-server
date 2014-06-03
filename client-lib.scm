;;; helpers for notifications (state udp-multicast socket helpers)
(use test medea)

(include "incubator.scm")
(import incubator)

;; ==================== notifications ====================


(begin
  (define make-notification cons)
  (define notification-path car)
  (define notification-value cdr))

(define (packet->json packet)
  (assert (string? packet))
  (define packet-header "NOTIFY ")

  (if (string-prefix? packet-header packet)
      (values (read-json (string-drop packet (string-length packet-header))))
      #f))

;; returns a cons cell with <path> . <json-value> from packet
(define (json->notification json)
  (and json
       (make-notification (alist-ref 'variable json)
                          (alist-ref 'data json))))

(define (packet->notification packet) (json->notification (packet->json packet)))

(test-group
 "packet->json->notification"

 (test "empty json" '() (packet->json "NOTIFY {}"))
 (test "invalid header json" #f (packet->json "notify {}"))

 (test "simple json value"
       `("/path" . ((a . 1)))
       (json->notification `((variable . "/path")
                             (data . ((a . 1))))))

 (test "empty notification (just to be clear)"
       (make-notification #f #f)
       (json->notification '()))

 (test "simple packet->notification"
       `("path" . ((a . 1)))
       (packet->notification "NOTIFY {\"variable\":\"path\",\"data\":{\"a\":1}}"))
 )


(define (state-volume state)        (get-in state "/v1/player/volume" 'value))
(define (state-playing state)       (get-in state "/v1/player/current"))
(define (state-playing-title state) (get-in (state-playing state) 'title))
(define (state-paused? state)       (get-in (state-playing state) 'paused))
(define (state-pos state)           (get-in state "/v1/player/pos" 'pos))
(define (state-total state)         (get-in state "/v1/player/pos" 'duration))

(test-group
 "player state"

 (test "Jackson" (state-playing-title
                  `(("/v1/player/current" .
                     ((title . "Jackson")
                      (turi . "tr://10.0.0.29:5055/v1/t2s?type=wimp&id=4124228"))))))

 (test #f (state-paused? '(("/v1/player/play" . ((foo . 1))))))
 (test #f (state-paused? '(("/v1/player/play" . ()))))

 (test 12 (state-volume `(("/v1/player/volume" . ((value . 12))))))

 (let ((state `(("/v1/player/pos" . ((pos . 5) (duration  . 10))))))
   (test 5  (state-pos state))
   (test 10 (state-total state))))

;; ==================== addressing ====================

(use socket)
(define (ip4-address name)
  (find (lambda (x) (= af/inet (addrinfo-family x)))
        (address-information name #f)))
;; (ip4-address "kth.lan")


;; hack to find your own ip address on the lan. we need this so we can
;; match against incoming multicast packets. (sender is not localhost,
;; so that won't work.)
(define (local-ip)
  (irregex-match-substring
   (irregex-search
    '(: "inet " (=> ip (or "192" "10") ;; assuming your LAN has this ip
                    (= 3 "." (** 1 3 numeric))) )
    (with-input-from-pipe "ifconfig|grep inet" read-string))
   'ip))

(define current-player (make-parameter (inet-address (local-ip) 5055)))
;;  (current-player)

;; is address our current player? check on IP only (not port, port is
;; always 5055 for notifications).
(define (current-player? addr)
  (equal? (sockaddr-address (current-player))
          (sockaddr-address addr)))


;; get the base REST url for our current server
(define (current-base-url path)
  (conc "http://"
        (sockaddr-address (current-player)) ":"
        (sockaddr-port (current-player)) "/v1"
        path))
;; (current-base-url "/path")


(parameterize ((current-player (inet-address "127.0.0.1" 5055)))
  (test-group
   "current-base-url"
   (test "http://127.0.0.1:5055/v1/foo" (current-base-url "/foo"))))
