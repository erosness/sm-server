(use spiffy medea matchable intarweb uri-common)

(include "broadcast.scm")
;; (include "debug.scm")

;; volume getter/setter (volume) => 50, (set! (volume) 100) => stores
;; new volume. getter/setters should work well with external variables
;; too (like dsp volume)
(define volume
  (let ((v 50))
    (getter-with-setter (lambda () v)
                        (lambda (new-volume)
                          ;; TODO: assert new-volume is [0 - 100]
                          (set! v new-volume)))))

(define (send-json x #!optional (status 'ok))
  (send-response headers: `((content-type "application/json"))
                 body: (conc (json->string x) "\n")
                 status: status))

(define (request-string!)
  (read-string (or (header-value 'content-length
                                 (request-headers (current-request))) 0)
               (request-port (current-request))))

(define (volume-handler)
  (case (request-method (current-request))
    [(GET) (send-json (volume))]
    [(PUT)
     (let ((vol (string->number (request-string!))))
       (set! (volume) vol)
       (udp-broadcast (conc "NOTIFY /player/volume\n\n" vol))
       (send-json (volume)))]))

(define (handler)
  (match (uri-path (request-uri (current-request)))
    [('/ "player" "volume") (volume-handler)]
    [else (send-json `((error . "unknown url")) 'not-found)]))


(vhost-map `((".*" . ,(lambda _ (handler)))))
;; (thread-start! (lambda () (start-server port: 5055)))
(eval-when (load) (start-server port: 5055))
