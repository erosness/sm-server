(include "debug-utils.scm")

(module* rest-info ()

(import chicken scheme data-structures srfi-1)

(use matchable)

;; local imports
(import restlib sm-config)

(define (empty-value)
  `((name . ,(unit-name(uid)))
    (cap . ,(capability (uid)))
    (uid  . ,(uid))))

(define-handler /v1/sm/info
  (lambda ()
    (empty-value)))
)
