(use spiffy json matchable intarweb uri-common)

(include "broadcast.scm")
(include "uri-handler.scm")
(include "dsp/dsp.scm")
(include "discovery.scm")

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

;; make an getter-with-setter that caches any set! calls and returns the
;; cache when the getter is called. this is useful for parameters that
;; are writeable but not readable (like our current dsp volume parameter).
(define (setter-with-cache initial #!optional (set-proc (lambda (v) (void))))
  (let ((v initial))
    (getter-with-setter (lambda () v)
                        (lambda (n) (set! v n) (set-proc n)))))

;; wrap getter/setter with a UDP NOTIFY for url
(define (with-setter-broadcast url accessor)
  (getter-with-setter accessor
                      (lambda (new)
                        (set! (accessor) new)
                        (udp-broadcast (conc "NOTIFY " url "\n\n" (->json new) "\n")))))

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
  `((player (mute      ,(setter-with-cache #f))
            (volume    ,(setter-with-cache 50 dsp-volume-set!))
            (eq        ,(setter-with-cache '#(0 0 0 0 0)))
            (radio      (info     ,(setter-with-cache "Classics"))
                        (current  ,(setter-with-cache "NRK MP3"))
                        (channels ,(setter-with-cache '("NRK P1" "NRK P2" "NRK Hordaland")))))))

;; TODO: clean this up a bit
(define *uris*
  (alist->hash-table
   (map (lambda (pair)
          (let ((url (car pair)))
            (cons url (with-setter-broadcast url (cdr pair)))))
        (uri-tree->alist *uri-tree* "" (cut conc <> "/" <>)))
   test: equal?))

(define (find-accessor uri #!optional (uris *uris*))
  (hash-table-ref/default uris uri #f))

;; main http request entry-point
(define (handler)
  (let ((uri (uri->string (make-uri path: (uri-path (request-uri (current-request)))))))
    (print "incoming " uri)
    (let ((handler (find-accessor uri)))
      (if handler
          (wrap-json handler)
          (send-json `#((error       . ,(conc "not found: " uri))
                        (valid-urls  . ,(hash-table-keys *uris*))) 'not-found)))))


(vhost-map `((".*" . ,(lambda _ (handler)))))
;; (thread-start! (lambda () (start-server port: 5055)))
;; (thread-start! (lambda () (start-discovery 5055 360)))
(eval-when (load)
           (thread-start! (lambda () (start-discovery 5055 360)))
           (start-server port: 5055))

;; for your repl pleasure:
;; you should see a UDP NOTIFY with this:
;; (set! ((find-accessor "/player/volume")) 21)

