;;; Cube Speaker service
;;;
;;; Defines REST interface to manipulate and play sound from a
;;; playqueue.
;;;
(use pefat)
(include "incubator.scm")

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

(use nrepl posix srfi-18)

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

;; TODO: make a prettier repl here. parley is pretty but stty's all
;; messed up on Android.
(define (repl*)
  ;; provide a repl on our network:
  (thread-start! (lambda () (nrepl (+ *server-port* 1))))
  ;; provide a repl on stdin:
  (nrepl-loop (make-nonblocking-stdin)
              (current-output-port)))
