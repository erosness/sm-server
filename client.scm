(use pefat)
(use http-client multicast test irregex srfi-18 socket utils
     medea
     fmt looper uri-common
     clojurian-syntax parley
     intarweb)

(include "incubator.scm")
(import incubator)
(include "client-lib.scm")

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



;; ==================== player ====================


;; not used (?)
;; (define (alist-map proc alist) (map (lambda (pair) (proc (car pair) (cdr pair))) alist))


(define (fold-notification n state)
  (alist-update (notification-path n)
                (notification-value n)
                state
                equal?))

(test-group
 "fold-notification"

 (test "simple notification fold"
       '(("/path" . ((value . 1))))
       (fold-notification (json->notification `((variable . "/path")
                                                (data . ((value . 1))))) '()))

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


(define (state-display state)
  (define (->fx x) (and x (inexact->exact (round x))))
  (conc " ♪" (->fx (state-volume state)) "%"
        " " (if (state-paused? state) "\u25ae" "\u25b6")
        " " (->fx (state-pos state)) "/" (->fx (state-total state))
        " " (fmt #f (ellipses "..." (trim 40 (dsp (state-playing-title state)))))
        " "
        ))

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

(define (columnize alist-list key)
  (fmt-join dsp (map (cut alist-ref key <>) alist-list) nl))

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
