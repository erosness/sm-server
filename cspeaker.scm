;;; Cube Speaker service
;;;
;;; Defines REST interface to manipulate and play sound from a
;;; playqueue.
;;;
(use pefat)

(include "debug-utils.scm")
(include "concurrent-utils.scm")

(include "process-cli.scm")
(include "player.scm")
(include "playqueue.scm")

(include "broadcast.scm")

(include "rest.scm")
(include "rest-pq.scm")


(include "dns-sd.scm")

(import rest)
(use restlib clojurian-syntax)

(define (start-rest-server! port)
  (thread-start!
   (lambda ()
     (define handler (->> (lambda () (json-handler))
                          (wrap-json)
                          (wrap-errors)))

     (vhost-map `((".*" . ,(lambda (continue) (handler)))))
     (start-server port: port))))

