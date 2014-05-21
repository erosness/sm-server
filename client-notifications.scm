(use http-client multicast test irregex
     fmt
     clojurian-syntax
     intarweb)


(define (alist-map proc alist) (map (lambda (pair) (proc (car pair) (cdr pair))) alist))


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
    (make-request method: (string->symbol (mm 'method))
                  uri: (uri-reference (mm 'uri))
                  headers: (read-headers in)
                  port: in)))

;; read a notify request
(define (read-notication-request str)
  (parameterize ((request-parsers (list notification-request-parsers)))
    (read-request (cond ((string? str) (open-input-string str))
                        (else str)))))


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
       (fold-notification (packet->notification "NOTIFY /path\r\n\r\n{\"value\":1}") '())))

(define s (multicast-listen-socket 5055))

;; for your repl pleasures:
;; (and (socket-receive-ready? s) (socket-receive-from s 2048))

;; our global server-state
(define state `())

;; do a GET request to server to find it's current value
(define (query-state path)
  (values (condition-case
           (with-input-from-request
            (conc "http://localhost:5055/v1" path)
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

(for-each
 (lambda (fail)
   (let ((expected (car fail))
         (actual (cdr fail)))
     (fmt #t (columnar (dsp expected) "|" (dsp actual)))))
 (filter (lambda (pair) (not (equal/alist? (car pair) (cdr pair)))) (check-state state)))

;; update server-state based on "packet"
(define (fold-server-state packet)
  (set! state (fold-notification (packet->notification packet) state)))

(begin
  (handle-exceptions e e (thread-terminate! notify-thread))
  (define notify-thread
    (thread-start!
     (->> (lambda ()
            (receive (packet addr) (socket-receive-from s 2048)
              ;; TODO: check addr is our current server
              (pp `(update ,(packet->notification packet)))
              (fold-server-state packet)))
          (loop/socket-timeout)
          (loop)
          (with-socket s)))))
;; (thread-terminate! notify-thread)
;; (thread-state notify-thread)
