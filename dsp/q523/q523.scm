(module q523 *

(import chicken scheme foreign)
(use srfi-4)

#>
#include "fxconv.c"
<#

(define fp->q523! (foreign-lambda void "To523" u8vector float))

(define (fp->q523 num)
  (let ((buff (make-u8vector 4)))
    (fp->q523! buff num)
    buff))

)
