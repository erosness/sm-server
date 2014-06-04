;;; helpers for broadcasting alerts to peers.
;;;
;;; === glossary ===
;;; broadcast: any UDP multicase/broadcast message
;;;
;;; change-message: a HTTP-like packet describing a change in the
;;; statemap

(module notify (send-notification)

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

(define *notify-connections* '())

(define (send-notification path body port)
  (let ((msg (change-message path body port)))

    ;; run through all current notify TCP connections
    ;; TODO: make this thread-safe
    (set! *notify-connections*
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

           *notify-connections*))

    ;; standard UDP multicast
    (udp-multicast msg)))

;; handler for TCP notification listeners. this is an offer for client
;; outside of our LAN/multicast range.
(define-handler /v1/player/notify
  (lambda ()
    (let ((port (response-port (current-response))))
      ;; add out-port to our pool
      ;; TODO: make this thread-safe
      (set! *notify-connections*
            (cons port *notify-connections*))
      ;; TODO: exit this somehow
      (let loop ()
        (thread-sleep! 5)
        ;; if connection has been deleted, let's exit our thread
        (if (find (lambda (x) (eq? x port)) *notify-connections*)
            (loop)))

      ;; this is dirty. we actually want to exit this thread
      ;; immediately, but that will clean up TCP connections and send a
      ;; HTTP respone etc so we can't do that. at this point, the
      ;; connection is probably terminated by the client anyway.
      (error "exiting thread for port " port))))


(include "notify.tests.scm")
)
