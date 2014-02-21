(use spiffy intarweb uri-common
     srfi-69 restlib test
     clojurian-syntax)


(define *uris* (make-hash-table))
(define (define-handler url thunk)
  (assert (string? url))
  (hash-table-set! *uris* url thunk))

(include "player.scm")

(include "rest-tone.scm")
(include "rest-wimp.scm")


(define /play
  (let ((cache #f))
    (lambda ()
      (let ((json (current-json)))
        (if json
            (let ((song (alist-ref 'turi json)))
              (begin (play! (play-command song))
                     (print "playing " song)
                     (set! cache song)
                     `((status . "ok"))))
            cache)))))


(define-handler "/play" /play)

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
;; (hash-table->alist *uris*)
