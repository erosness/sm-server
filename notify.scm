;;; helpers for broadcasting alerts to peers.
;;;
;;; === glossary ===
;;;
;;; change-message: a HTTP-like packet describing a change in the
;;; statemap

(module notify (send-notification make-notify-handler)

(import chicken scheme ports data-structures)
(use socket posix intarweb spiffy medea srfi-1 srfi-18 restlib)


;; create a message that represents `path` changing into `body`. port
;; specifies "owner" so change-message origins can be identified
;; (usually port-number of running service).
;;
;; path is string. body is medea-json. port is number.
(define (change-message path body port)
  (assert (not (string? body))) ;; <-- just to make sure we catch api
                                ;; change (no more strings - use
                                ;; alists!)
  (conc "NOTIFY "
        (json->string `((variable . ,path)
                        (owner . ((port . ,port)))
                        (data . ,body)
                        (echo . ,(cond ((current-request) =>
                                        (lambda (req)
                                          (header-value 'echo (request-headers req))))
                                       (else #f)))))))

(define *notify-connections* '())

(define (send-notification path body #!optional
                           (connections (getter-with-setter
                                         (lambda () *notify-connections*)
                                         (lambda (new) (set! *notify-connections* new))))
                           (port (server-port)))
  (let ((msg (change-message path body port)))
    (for-each
     (lambda (port)
       (condition-case
           (begin
             (display msg port)
             (display #\newline port)
             (flush-output port))
         (e (exn i/o net)
            (print "invalid notify connection " port))))
     (connections))))

(define (make-notify-handler #!optional
                             (connections
                              (getter-with-setter (lambda () *notify-connections*)
                                                  (lambda (new) (set! *notify-connections* new)))))
  (print "********* make-notify-handler")
  (define (notify-handler)
    (let ((port (response-port (current-response))))
      ;; add out-port to our pool
      ;; TODO: make this thread-safe
      (set! (connections) (cons port (connections)))

      ;; HACK: Block the thread here until we can read from it,
      ;; since the client should never write anything through the
      ;; socket we assume that when data becomes available it's the
      ;; #eof character. Once that has been received we remove this
      ;; port from the active connections.
      (thread-wait-for-i/o! (port->fileno port) #:input)
      (print  port " has been closed, removing connection")


      ;; TODO: thread-safe before commit!!
      (set! (connections)
            (filter (lambda (p) (not (equal? p port))) (connections)))

      ;; this is dirty. we actually want to exit this thread
      ;; immediately, but that will clean up TCP connections and send a
      ;; HTTP respone etc so we can't do that. at this point, the
      ;; connection is probably terminated by the client anyway.

      ;; if we return here, we get "cannot write to socket - broken
      ;; pipe" because Spiffy is trying to reply to an HTTP connection
      ;; that we have hijacked.
      ;;
      ;; TODO: remove the ugly Spiffy error
      ;; message but allow Spiffy to clean up its thread-count
      ;; variable.
      ))



  ;; return a named procedure
  notify-handler)

;; handler for TCP notification listeners. this is an offer for client
;; outside of our LAN/multicast range.
;; TODO: put this elsewhere.
(define-handler /v1/player/notify (make-notify-handler))


(include "notify.tests.scm")
)
