;;; Cube Speaker service
;;;
;;; Defines REST interface to manipulate and play sound from a
;;; playqueue.
;;;
(use pefat)
(include "incubator.scm")
(include "store.scm")

(include "closing-http-client.scm")

(include "debug-utils.scm")
(include "concurrent-utils.scm")
(include "notify.scm")

(include "rest.scm")

(include "process-cli.scm")

(include "player.scm")
(include "playqueue.scm")

(include "dns-sd.scm")

;; rest plugin layers
(include "rest-audio.scm")
(include "rest-player.scm")
(include "rest-version.scm")

(include "rest-icon.scm")

(use nrepl posix srfi-18 spiffy)

(define (make-nonblocking-stdin)

  (define cip (current-input-port))
  (set-buffering-mode! cip #:none) ;; <-- important!

  (make-input-port (lambda ()
                     (let loop ()

                       (if (char-ready? cip)
                           (read-char cip)
                           (begin
                             (thread-wait-for-i/o! (port->fileno cip) #:input)
                             (loop)))))
                   (lambda () (char-ready? cip))
                   (lambda () (close-input-port cip))))

;; provide a repl on our network
(define (start-nrepl #!optional (port (+ (server-port) 1)))
  (thread-start! (lambda () (nrepl port))))

;; TODO: make a prettier repl here. parley is pretty but stty's all
;; messed up on Android.
(define (repl*)
  ;; provide a repl on stdin:
  (nrepl-loop (make-nonblocking-stdin)
            (current-output-port)))


(define (random-suffix)
  (string-join (map (lambda _ (->string (random 10))) (iota 4)) ""))

;; Annonce a cube-pq service with a txt record holding the service's
;; friendly name and icon. Also register a route for changing the txt
;; record. Upon change the service is stopped and restarted with the
;; new txt record. We have to do this due to limitations in
;; avahi-publish.
(define (register-pq-with-icon-store nickname port #!optional (type "record-player"))

  (import rest)
  (define txt-record (string->symbol (conc "dns-" nickname "-" port)))

  (define player-store (make-store txt-record))
  (if (not (player-store))
   (player-store `((name . ,nickname))))

  ;; Publish with a 4 digit random suffix, used by clients to identify
  ;; this service, regardless of name and ip
  (let ((publish-name (conc nickname "-" port)))
    (define dns-sd-unregister! (dns-sd-register publish-name port service-type/cube-pq txt-record type))

    ;; TODO: don't hardcode /v1/player
    (define-handler /v1/player
      (lambda ()
        (if (current-json)
            ;; Should not be able to change type
            (let ((new-record (alist-delete 'type (current-json))))
              (player-store new-record)
              (dns-sd-unregister!)
              (set! dns-sd-unregister!
                    (dns-sd-register publish-name port service-type/cube-pq txt-record type))))
        (or (cons `(type . ,type)
                  (player-store)) '())))

    dns-sd-unregister!))
