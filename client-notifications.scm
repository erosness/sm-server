(use pefat)
(use http-client multicast test irregex srfi-18 socket utils
     medea
     fmt looper uri-common
     clojurian-syntax parley
     intarweb)

;; ==================== repl helpers ====================

(define original-input-port (current-input-port))
(define original-output-port (current-output-port))


;; ==================== info view ====================


;; (last line is simple current "context line")
(define (context-line) "n/a")

;; display bottom status-line (will get updated even though we print
;; lines with info). hopefully thread-safe. with an argument, update
;; the current state as well.
(define (liner #!optional str)
  (if str (set! context-line str))
  (display "\r")
  (display (context-line))
  (flush-output))

;; do a carriage return before print
(define (info x . args)
  (parameterize ((current-output-port original-output-port))
    ;; clear line
    (display "\r")
    (display (make-string (string-length (context-line)) #\space))
    (display "\r")
    ;; allow serializing obj data as first argument
    (if (string? x) (print* x) (write x))
    ;; print rest:
    (apply print args)
    (liner)))


;; ==================== addressing ====================

(define (ip4-address name)
  (find (lambda (x) (= af/inet (addrinfo-family x)))
        (address-information name #f)))
;; (ip4-address "kth.lan")

;; (command-line-arguments '("kth.lan"))
;; (command-line-arguments '("moo.lan"))

(if (not (= 1 (length (command-line-arguments))))
    (error "usage: player-address"))

;;  (current-player)
(define current-player
  (make-parameter
   (inet-address
    (sockaddr-address
     (addrinfo-address (ip4-address (first (command-line-arguments)))))
    ;; TODO: take port too from cli
    5055)))

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


;; ==================== player ====================


;; not used (?)
;; (define (alist-map proc alist) (map (lambda (pair) (proc (car pair) (cdr pair))) alist))

;; a nicer request-printer
(define (request->list r)
  (list (request-method r)
        (uri->string (request-uri r))
        (list (request-major r) (request-minor r))
        (headers->list (request-headers r))))


(define token-sre '(+ (~ "()<>@,;:\\\"/[]?={}\t ")))

;; almost HTTP compliant, except we don't have a proper HTTP/1.1
;; header line.
(define (notification-request-parsers line in)
  (let* ((m (irregex-match `(: (=> method ,token-sre) (+ space)
                               (=> uri (+ (~ blank))) (* space))
                           line))
         (mm (lambda (lbl) (irregex-match-substring m lbl))))
    (and m
         (make-request method: (string->symbol (mm 'method))
                       uri: (uri-reference (mm 'uri))
                       headers: (read-headers in)
                       port: in))))

;; read a notify request
(define (read-notication-request str)
  (parameterize ((request-parsers (list notification-request-parsers)))
    (handle-exceptions e (begin (pp (condition->list e)) #f)
     (read-request (cond ((string? str) (open-input-string str))
                         (else str))))))


(test-group
 "notification-request-parsers"

 (test "method symbol" 'NOTIFY (request-method (read-notication-request "NOTIFY /a\r\n")))
 (test "uri string"
       "/path" (->> "A /path\r\n" (read-notication-request) (request-uri) (uri->string)))
 (test "ready to read payload"
       "body"
       (->> "NOTIFY /path\r\nvalue: hei\r\n\r\nbody"
            (read-notication-request)
            (request-port)
            (read-all))))


(begin
  (define make-notification cons)
  (define notification-path car)
  (define notification-value cdr))


;; returns a cons cell with <path> . <json-value> from packet
(define (packet->notification packet)
  (assert (string? packet))
  (and-let* ((r (read-notication-request packet))
             (method (request-method r))
             (_ (eq? 'NOTIFY method))
             (p (uri->string (request-uri r))))
    (make-notification p (read-json (request-port r)))))


(test-group
 "packet->notification"

 (test "empty json" `("x" . ()) (packet->notification "NOTIFY x\r\n\r\n{}"))

 (test "simple json value"
       `("/path" . ((a . 1)))
       (packet->notification "NOTIFY /path\r\n\r\n{\"a\" : 1}"))

 (test "check http method" #f (packet->notification "PUT /path\r\n\r\n{\"a\" : 1}"))
 )

(define (fold-notification n state)
  (alist-update (notification-path n)
                (notification-value n)
                state
                equal?))

(test-group
 "fold-notification"

 (test "simple notification fold"
       '(("/path" . ((value . 1))))
       (fold-notification (packet->notification "NOTIFY /path\r\n\r\n{\"value\":1}") '()))

 (test "notification overwrite"
       '(("/path" . 2))
       (fold-notification (make-notification "/path" 2) '(("/path" . 1))))
 )


(define s (multicast-listen-socket 5055))

;; for your repl pleasures:
;; (and (socket-receive-ready? s) (socket-receive-from s 2048))

;; our global server-state
(define state `())

;; do a GET request to server to find it's current value
(define (query-state path)
  (values (condition-case
           (with-input-from-request
            (current-base-url path)
            #f read-json)
           ((exn http client-error) #f))))

;; run through all paths in state and request their current value.
;; these should be (approximately) equal to the last
;; notification-event received from the same service.
(define (compare-state state)
  (map (lambda (pair)
         (let* ((path (car pair))
                ;; value of last notification:
                (notification (cdr pair))
                ;; re-query server (same url!!)
                (actual (query-state path)))
           (cons notification actual)))
       state))

;; run through all paths and get their statuses
(define (get-states paths)
  (let loop ((paths paths)
             (r '()))
    (if (pair? paths)
        (let ((path (car paths)))
          (loop (cdr paths) (cons (make-notification path (query-state path)) r)))
        r)))

;; (pp (get-states '("/volume" "/eq" "/mute" "/player/play")))

;; for debugging
(define (urls host)
  (->>  (with-input-from-request host #f read-json)
        (alist-ref 'valid-urls)
        (vector->list)
        ((flip sort) string>?)))
;; (pp (urls "http://localhost:5055/_urls"))

(define (alist=? a b)
  (define (key>? a b)  (string>= (->string (car a)) (->string (car b))))

  (and (list? a) (list? b)
       (equal? (sort a key>?)
               (sort b key>?))))

(define (equal/alist? a b)
  (or (equal? a b) (alist=? a b)))

(test-group
 "equal/alist?"
 (test #t (equal/alist? 1 1))
 (test #f (equal/alist? 1 0))
 (test #t (equal/alist? "a" "a"))


 (test #t (equal/alist? `((a . 1) (b . 2))
                        `((b . 2) (a . 1))))

 (test #f (equal/alist? `((a . 1) (b . 1))
                        `((a . 1) (b . 2)))))

;; like alist-ref, but we're comparing keys as string and doing it
;; recursively. also doen't vomit if state if #f. kinda like clojure,
;; I guess! you'll love this, Peder.
(define (get-in state . keys)
  (let loop ((state state)
             (keys keys))
    (if (pair? keys)
        (and state
             (loop (alist-ref (car keys) state equal?) (cdr keys)))
        state)))

(test-group
 "get-in"
 (test #f (get-in '() 'a))
 (test #f (get-in '() 'a 'b))

 (test 1 (get-in '((a . 1)) 'a))
 (test #f (get-in '((a . ((b . 0)))) 'a 'x))

 (test 2 (get-in '((a . ((b . 2)))) 'a 'b)))


(define (state-paused? state)  (not (get-in state "/v1/player/play")))
(define (state-volume state)        (get-in state "/v1/player/volume" 'value))
(define (state-playing state)       (get-in state "/v1/player/current"))
(define (state-playing-title state) (get-in (state-playing state) 'title))
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

(define (state-display state)
  (define (->fx x) (and x (inexact->exact (round x))))
  (conc " ♪" (->fx (state-volume state)) "%"
        " " (if (state-paused? state) "\u25ae" "\u25b6")
        " " (->fx (state-pos state)) "/" (->fx (state-total state))
        " " (fmt #f (ellipses "..." (trim 40 (dsp (state-playing-title state)))))
        " "
        ))

;; (for-each
;;  (lambda (fail)
;;    (let ((expected (car fail))
;;          (actual (cdr fail)))
;;      (fmt #t (columnar (dsp expected) "|" (dsp actual)))))
;;  (filter (lambda (pair) (not (equal/alist? (car pair) (cdr pair)))) (check-state state)))

;; update server-state based on "packet"
(define (fold-server-state packet)
  (cond ((packet->notification packet) =>
         (lambda (notification) (set! state (fold-notification notification state))))
        (else (info `(ignoring: ,packet)))))

(begin
  (handle-exceptions e e (thread-terminate! notify-thread))
  (define notify-thread
    (thread-start!
     (->> (lambda ()
            ;; so that nrepl don't steal our original terminal output
            ;; prompt
            (receive (packet addr) (socket-receive-from s 2048)
              ;; TODO: check addr is our current server
              (if (current-player? addr)
                  (begin
                    (fold-server-state packet)
                    (info (fmt #f (pretty `(update ,(packet->notification packet)))))))))
          (loop/socket-timeout)
          (loop)
          (with-socket s)))))
;; (thread-terminate! notify-thread)
;; (thread-state notify-thread)

(liner (lambda () (state-display state)))

;; flush liner in the original terminal port
(define (flush-liner)
  (parameterize ((current-output-port original-output-port))
    (liner)))

(define (display-pq pq)
  (fmt #t
       nl

       (tabular (fmt-join (cut pad/left 3 <>) (iota (length pq)) nl)
                " "
                (columnize pq 'id)
                " "
                (columnize pq 'turi))
       nl))

;; (display-pq (vector->list (query-state "/player/pq")))

(use nrepl)
(nrepl 1234)
