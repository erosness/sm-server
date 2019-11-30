(include "debug-utils.scm")

(module* rest-info ()

(import chicken scheme data-structures srfi-1 srfi-69)

(use matchable)

;; local imports
(import restlib sm-config)

(define (hash-capability cap uid)
    (map (lambda (e)
      `((fid . ,(string-hash (conc uid e)))
        (cap . ,e))) cap ))

(define (unit-info)
  `((name . ,(unit-name))
    (cap  . ,(list->vector(hash-capability (capability) (uid))))
    (uid  . ,(uid))))

(define-handler /v1/sm/info
  (lambda ()
    (unit-info)))
)
