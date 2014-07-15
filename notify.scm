;;; helpers for broadcasting alerts to peers.
;;;
;;; === glossary ===
;;; broadcast: any UDP multicase/broadcast message
;;;
;;; change-message: a HTTP-like packet describing a change in the
;;; statemap

(module notify (send-notification make-notify-handler)

(import chicken scheme ports data-structures)
(use socket intarweb spiffy medea multicast srfi-1 srfi-18 restlib)

(import multicast)

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

;; TODO: clean this bit up a bit. we don't need heavyweight defaults
;; like this
(define *notify-connections* '())

(define (send-notification path body port #!optional
                           (connections (getter-with-setter
                                         (lambda () *notify-connections*)
                                         (lambda (new) (set! *notify-connections* new)))))
  (let ((msg (change-message path body port)))

    ;; run through all current notify TCP connections
    ;; TODO: make this thread-safe
    (set! (connections)
          (filter
           (lambda (port)

             (condition-case
              (begin
                (display msg port)
                (display #\newline port)
                (flush-output port)
                #t) ;; <-- filter in
              (e (exn i/o net)
                 (print "removing notify connection " port)
                 #f) ;; <-- filter out
              ))

           (connections)))

    ;; standard UDP multicast
    (udp-multicast msg)))

(define (make-notify-handler #!optional
                             (connections
                              (getter-with-setter (lambda () *notify-connections*)
                                                  (lambda (new) (set! *notify-connections* new)))))
  (define (notify-handler)
    (let ((port (response-port (current-response))))
      ;; add out-port to our pool
      ;; TODO: make this thread-safe
      (set! (connections) (cons port (connections)))

      (let loop ()
        (thread-sleep! 5)
        ;; if connection has been deleted, let's exit our thread
        (if (find (lambda (x) (eq? x port)) (connections))
            (loop)))

      ;; this is dirty. we actually want to exit this thread
      ;; immediately, but that will clean up TCP connections and send a
      ;; HTTP respone etc so we can't do that. at this point, the
      ;; connection is probably terminated by the client anyway.

      ;; if we return here, we get "cannot write to socket - broken
      ;; pipe" because Spiffy is trying to reply to an HTTP connection
      ;; that we have hijacked. I wonder if thread-terminate! will
      ;; leak anything?
      (thread-terminate! (current-thread))))

  ;; return a named procedure
  notify-handler)

;; handler for TCP notification listeners. this is an offer for client
;; outside of our LAN/multicast range.
;; TODO: put this elsewhere.
(define-handler /v1/player/notify (make-notify-handler))


(include "notify.tests.scm")
)
