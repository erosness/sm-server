(use spiffy intarweb uri-common
     srfi-69 restlib
     clojurian-syntax)

(include "debug-utils.scm")

(include "concurrent-utils.scm")
(include "process-cli.scm")

(include "broadcast.scm")
(include "player.scm")
(include "playqueue.scm")

(include "rest.scm")
(include "rest-tone.scm")
(include "rest-notes.scm")
(include "rest-wimp.scm")
(include "rest-pq.scm")
(include "rest-player.scm")

(import rest)

(define (find-accessor uri #!optional (uris *uris*))
  (hash-table-ref/default uris uri #f))

(define (json-handler)
  (let ((uri (uri->string (make-uri path: (uri-path (request-uri (current-request)))))))
    (let ((handler (find-accessor uri)))
      (if handler
          (handler)
          `((error       . ,(conc "not found: " uri))
            (valid-urls  . ,(list->vector (hash-table-keys *uris*))))))))

(define handler (->> (lambda () (json-handler))
                     (wrap-json)
                     (wrap-errors)))

(vhost-map `((".*" . ,(lambda (continue) (handler)))))

;; for your repl pleasure:
;; (define thread (thread-start! (lambda () (start-server port: 5055))))
;; (pp (hash-table->alist *uris*))

