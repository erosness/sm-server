;;; Various helper utils for our rest server.


(use clojurian-syntax           ;; util
     spiffy intarweb uri-common ;; web
     medea                      ;; json
     )

;;; ******************** misc ********************

(define (make-limited-input-port limit port)
  (make-input-port (let ((count limit))
                     (lambda () (if (< 0 count)
                               (begin (set! count (sub1 count))
                                      (read-char port))
                               #!eof)))             ;; read
                   (lambda () (char-ready? port))        ;; ready?
                   (lambda () (close-input-port port))))

;; (read-string #f) on (request-port request) will block because we
;; never get #!eof (tcp connection is still open). so we need a
;; cut-off (which is the content-length in the request headers)
(define (request-payload-port request)
  (make-limited-input-port
   (or (header-value 'content-length (request-headers (current-request))) 0)
   (request-port request)))

;; slurp entire request payload into string
(define (request-string!)
  (read-string (or (header-value 'content-length
                                 (request-headers (current-request))) 0)
               (request-port (current-request))))

(define (send-json x #!optional (status 'ok))
  (send-response headers: `((content-type "application/json"))
                 body: (with-output-to-string (lambda () (write-json x)))
                 status: status))

;; send HTTP "100 Continue" (iff client-headers Expect: 100-continue)
;; without this, some clients wait before sending PUT/POST payload
(define (send-continue?!)
  (if (header-value 'expect (request-headers (current-request)))
      (write-response
       (make-response port: (response-port (current-response))
                      status: 'continue))))

;; call handler with an exception handler, and log error to request
;; response instead of stderr.
(define (wrap-errors handler)
  (lambda ()
   (handle-exceptions exn
     (send-response status: 'bad-request
                    body: (conc ((condition-property-accessor 'exn 'message) exn)
                                ": " ((condition-property-accessor 'exn 'arguments) exn) "\n"
                                ;; condition->list is similar to
                                ;; print-call-chain but it only print
                                ;; chain for exn, so it's nicer.
                                (with-output-to-string (lambda () (pp (condition->list exn))))))
     (handler))))

;; false if request is GET or if wrap-json hasn't been invoked
(define current-json (make-parameter #f))

;; convert response to json (alist => json object, etc) and convert
;; request payload (if any) from json.
(define (wrap-json handler)
  (lambda ()
    ;; on GET, call handler with no args. on PUT, call it with parsed
    ;; json input. in both cases, return json-representation of
    ;; handler's return value
    (case (request-method (current-request))
      [(GET) (send-json (handler))]
      [(PUT POST)
       (let* ((req-string (request-string!))
              (json (or (read-json req-string)
                        (error "invalid json" req-string))))
         (current-json json) ;; <-- never #f
         (send-json (handler)))]
      (else (error (conc "unsupported method " (request-method (current-request))))))))

(define (wrap-continue handler)
  (lambda ()
    (case (request-method (current-request))
      ((PUT POST) (send-continue?!)))
    (handler)))

;; convenience for picking out query parameters
(define (current-query-param key)
  (->> (current-request)
       (request-uri)
       (uri-query)
       (alist-ref key)))


;;; ******************** pagination ********************



;; if cnt is #f, leave lst unchanged. if cnt if greater than length of
;; list, pretend it's equal to the length of the list.
(define (maybe-drop cnt lst) (if cnt
                                 (if (> cnt (length lst))
                                     '()
                                     (drop lst cnt))
                                 lst))
(define (maybe-take cnt lst) (if (and cnt (>= (length lst) cnt)) (take lst cnt) lst))


;; currently data must be list
(define (paginate data limit offset)

  (define (get-counter data)
    (cond ((list? data) length)
          (else (error "cannot paginate " data))))

  (define (crop data)
    (cond ((list? data) (->> data
                             (maybe-drop offset)
                             (maybe-take limit)
                             (list->vector)))
          (else (error "cannot crop " data))))

  `((limit  . ,limit)
    (offset . ,offset)
    (total  . ,((get-counter data) data))
    (items  . ,(crop data))))


(define (current-limit)
  (or (and-let* ((str (current-query-param 'limit)))  (string->number str)) 10))
(define (current-offset)
  (or (and-let* ((str (current-query-param 'offset))) (string->number str)) 0))