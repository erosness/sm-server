

(module nics (nics)

(import chicken scheme foreign)

(define-external (nic_add (scheme-object lst)
                          (c-string name)
                          (c-string adr))
  scheme-object
  (cons (cons (string->symbol name) adr) lst))

#>
#include "nics.native.c"
<#

(define (nics)
  ((foreign-safe-lambda scheme-object nics scheme-object) '()))

)
