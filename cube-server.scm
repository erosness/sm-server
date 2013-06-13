(use spiffy json matchable intarweb uri-common)

(include "broadcast.scm")
(include "uri-handler.scm")
;; (include "debug.scm")

(define (->json x)
  (with-output-to-string (lambda () (json-write x))))

(define (send-json x #!optional (status 'ok))
  (send-response headers: `((content-type "application/json"))
                 body: (conc (->json x) "\n")
                 status: status))

(define (request-string!)
  (read-string (or (header-value 'content-length
                                 (request-headers (current-request))) 0)
               (request-port (current-request))))

;; send HTTP "100 Continue" (iff client-headers Expect: 100-continue)
;; without this, some clients wait before sending PUT/POST payload
(define (send-continue?!)
  (if (header-value 'expect (request-headers (current-request)))
      (write-response
       (make-response port: (response-port (current-response))
                      status: 'continue))))


(define (make-accessor #!optional (initial (void)))
  (let ((v initial))
    (getter-with-setter (lambda () v)
                        (lambda (n) (set! v n)))))


(define (wrap-json accessor)
  (case (request-method (current-request))
    [(GET) (send-json (accessor))]
    [(PUT)
     (send-continue?!)
     (handle-exceptions exn
       (send-response status: 'bad-request
                      body: (conc ((condition-property-accessor 'exn 'message) exn)
                                  "\n"
                                  ((condition-property-accessor 'exn 'location) exn)
                                  (with-output-to-string
                                    (lambda () (print-call-chain)))))
       ;; call the actual setter procedure:
       (set! (accessor) (json-read (open-input-string (request-string!))))
       (send-response status: 'ok))]))

(define *uri-tree*
  `((player (mute      ,(make-accessor #f))
            (volume    ,(make-accessor 50))
            (eq        ,(make-accessor '#(0 0 0 0 0)))
            (radio      (info    ,(make-accessor "Classics"))
                        (current ,(make-accessor "NRK MP3"))))))

(define *uris* (alist->hash-table
                (uri-tree->alist *uri-tree* "" (cut conc <> "/" <>))
                test: equal?))

(define (find-getter uri #!optional (uris *uris*))
  (hash-table-ref/default uris uri #f))

(define (handler)
  (let ((uri (uri->string (make-uri path: (uri-path (request-uri (current-request)))))))
    (print "incoming " uri)
    (let ((handler (find-getter uri)))
      (if handler
          (wrap-json handler)
          (send-json `#((error       . ,(conc "not found: " uri))
                        (valid-urls  . ,(hash-table-keys *uris*))) 'not-found)))))


(vhost-map `((".*" . ,(lambda _ (handler)))))
;; (thread-start! (lambda () (start-server port: 5055)))
(eval-when (load) (start-server port: 5055))
