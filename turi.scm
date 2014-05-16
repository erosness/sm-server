(module turi ( *audio-hosts*
               register-audio-host!
               define-turi-adapter
               )

(import chicken scheme)
(use intarweb test uri-common data-structures restlib)

(import rest)




;; provide an API for audio hosts / providers to plug into.
(define *audio-hosts* `())
(define (set-audio-host! host handler)
  ;; add host to audio hosts
  (set! *audio-hosts* (alist-update host handler *audio-hosts* equal?)))

(define (turi-handler type id)
  (cond ((assoc type *audio-hosts*) =>
         (lambda (pair) ((cdr pair) id)))
        (else (error "no turi adapter for " type))))

(define-handler /v1 /t2s (argumentize (lambda (t i) (turi-handler t i)) 'type 'id))

(define (make-turi-creator type)
  (lambda (id)
    (assert (or (string? id) (number? id)))
    (let ((id (conc id)))
      ;; produce & separators, not ;
      (parameterize ((form-urlencoded-separator "&"))
        (uri->string (update-uri (current-host)
                                 scheme: 'tr
                                 path: `(/ "v1" "t2s")
                                 query: `((type . ,type)
                                          (id . ,id))
                                 ;; bug workaround (uri-common):
                                 port: (uri-port (current-host))))))))

(test-group
 "uri creator"
 (with-request
  ("/" `((host ("host" . 80))))
  (test "tr://host:80/t2s?type=debug&id=123"
        ((make-turi-creator "debug") "123"))
  (test "tr://host:80/t2s?type=debug&id=abc"
        ((make-turi-creator "debug") "abc"))))


(define (register-audio-host! name handler)
  (set-audio-host! name handler)
  (make-turi-creator name))

(define-syntax define-turi-adapter
  (syntax-rules ()
    ((_ variable name id->suri)
     (begin (define variable (register-audio-host! name id->suri))))))


)
