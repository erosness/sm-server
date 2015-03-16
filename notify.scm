;;; helpers for broadcasting alerts to peers.
;;;
;;; === glossary ===
;;;
;;; change-message: a HTTP-like packet describing a change in the
;;; statemap

(module notify (send-notification make-notify-handler)

(import chicken scheme ports data-structures)
(use socket posix intarweb spiffy medea srfi-1 srfi-18 restlib extras)


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
       ;; we often get "port already closed" exceptions. TODO: remove
       ;; port from set if it's useless. I've seen this happen (it's
       ;; not thread-safe after all)
       (handle-exceptions e (begin (print
                                    "invalid notify connection " port ": "
                                    (condition->list e) ", removing")
                                   ;; TODO: make it thread-safe
                                   (set! (connections)
                                         (filter (lambda (p) (not (equal? p port))) (connections))))
                          (display msg port)
                          (display #\newline port)
                          (flush-output port)))
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

      ;; this part is tricky. we want to:
      ;;
      ;; - ignore any more data coming in from the client (clients can
      ;; use it to keep the TCP connection alive)
      ;;
      ;; - close the TCP connection when client closes it
      ;; - return back to spiffy for proper cleanup
      ;; - spiffy prints an error message which is ugly but otherwise fine
      ;;   (setting current-error-port does not help)
      ;; - response data is sent asynchronously
      ;; think of this code snippet as a TCP connection watchdog.
      (let ((port (request-port (current-request))))
        (let loop ()
          (thread-wait-for-i/o! (port->fileno port) #:input)
          (let ((r (read-char port)))
            (if (not (eof-object? r))
                (loop)))))

      ;; give power back to spiffy
      (pp `(info ,(current-thread) notify-connection-down ,(remote-address)))

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


(use posix)
;; return a value representing this cube-server instance.
;; this way if cube-server restarts clients can detect that they need
;; to refresh long running connections (notify)
(define-handler /v1/instance
  ;; Return as string, in case we want to change the value later
  (lambda () `((instance . ,(conc (current-process-id))))))


(include "notify.tests.scm")
)
