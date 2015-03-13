(module turi ( *audio-hosts*
               register-audio-host!
               define-turi-adapter
               )

(import chicken scheme)
(use intarweb test uri-common data-structures restlib)

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

(define (make-turi-creator type)
  (lambda (params)
    (assert (alist? params) "make-turi-creator: Not an alist" params)

    ;; produce & separators, not &;
    (parameterize ((form-urlencoded-separator "&"))
      (uri->string (update-uri (current-host)
                               scheme: 'tr
                               path: `(/ "v1" "t2s")
                               query: `((type . ,type)
                                        ,@params)
                               ;; workaround for
                               ;; https://tradio.adellica.com/cube/cube-server/issues/93
                               host: "127.0.0.1"
                               ;; bug workaround (uri-common):
                               port: (uri-port (current-host)))))))

(test-group
 "uri creator"
 (with-request
  ("/" `((host ("host" . 80))))
  (test "tr://host:80/v1/t2s?type=debug&id=123"
        ((make-turi-creator "debug") '((id . 123))))
  (test "tr://host:80/v1/t2s?type=debug&id=abc"
        ((make-turi-creator "debug") '((id . "abc"))))
  (test "tr://host:80/v1/t2s?type=debug&foo=bar&monkey=krish"
        ((make-turi-creator "debug") '((foo . "bar") (monkey . "krish"))))))


(define (register-audio-host! name handler)
  (set-audio-host! name handler)
  (make-turi-creator name))

(define-syntax define-turi-adapter
  (syntax-rules ()
    ((_ variable name id->suri)
     (begin (define variable (register-audio-host! name id->suri))))))
)
