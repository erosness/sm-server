(module turi ( *audio-hosts*
               register-audio-host!
               define-turi-adapter
               )

(import chicken scheme)
(use intarweb test uri-common data-structures restlib srfi-14)

(import rest)

;; TEMP!!
(use spiffy)


;; provide an API for audio hosts / providers to plug into.
(define *audio-hosts* `())
(define (set-audio-host! host handler)
  ;; add host to audio hosts
  (set! *audio-hosts* (alist-update host handler *audio-hosts* equal?)))

(define (turi-handler type params)
  (cond ((assoc type *audio-hosts*) =>
         (lambda (pair) ((cdr pair) params)))
        (else (error "no turi adapter for " type))))

(define-handler /v1/t2s (wrap-params
                         (lambda (params)
                           (or (and-let* ((t (alist-ref 'type params)))
                                 (turi-handler t params))
                               ;; Error
                               (error (conc "parameter type and/or id missing in "
                                            (uri->string (request-uri (current-request)))))))))

;; TODO: implement
(define alist? list?)


;; uri-common's uri-encode-string does not suffice because it won't
;; encode "\303" for example. let's be more strict and list allowed
;; characters instead.
(define (safe-uri-encode s)
  (uri-encode-string
   s
   (char-set-complement
    (char-set-union (ucs-range->char-set (char->integer #\a)
                                         (char->integer #\z))
                    (ucs-range->char-set (char->integer #\A)
                                         (char->integer #\Z))
                    char-set:digit
                    (char-set #\. #\/)))))

(define (alist->querystring alst #!optional (separator "&"))
  (string-intersperse
   (map (lambda (pair)
          (let ((name (car pair))
                (value (->string (cdr pair))))
            (conc name "=" (safe-uri-encode value))))
        alst)
   separator))

;; we need to use uri-generic here because uri-common serializes the
;; query-parameter without escaping "\303" and friends. this is bad.
(use (prefix uri-generic generic:))

(define (make-turi-creator type)
  (lambda (params)
    (assert (alist? params) "make-turi-creator: Not an alist" params)

    (generic:uri->string
     (generic:make-uri scheme: 'tr
                       host: (uri-host (current-host))
                       port: (uri-port (current-host))
                       path: `(/ "v1" "t2s")
                       ;; uri-generic uri's simply do raw
                       ;; string/lists for query-params:
                       query: (alist->querystring `((type . ,type)
                                                    ,@params))))))


(test-group
 "uri creator"
 (with-request
  ("/" `((host ("server.header" . 80))))
  (test "tr://server.header:80/v1/t2s?type=debug&id=123"
        ((make-turi-creator "debug") '((id . 123))))
  (test "tr://server.header:80/v1/t2s?type=debug&id=abc"
        ((make-turi-creator "debug") '((id . "abc"))))
  (test "tr://server.header:80/v1/t2s?type=debug&foo=bar&monkey=krish"
        ((make-turi-creator "debug") '((foo . "bar") (monkey . "krish"))))
  (test "tr://server.header:80/v1/t2s?type=debug&evil=%C3"
        ((make-turi-creator "debug") `((evil . "\303"))))))


(define (register-audio-host! name handler)
  (set-audio-host! name handler)
  (make-turi-creator name))

(define-syntax define-turi-adapter
  (syntax-rules ()
    ((_ variable name id->suri)
     (begin (define variable (register-audio-host! name id->suri))))))
)
