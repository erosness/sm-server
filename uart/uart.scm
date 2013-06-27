(module uart *
(import chicken scheme foreign)

(use posix)

#>
#include "uart-interface.c"
<#

(define B115200 (foreign-value "B115200" int))

(define set-uart-config (foreign-lambda int "set_interface_attribs" int int int))

(define (uart-open path
                   #!optional
                   (speed B115200)
                   (parity 0))
  (define fd (file-open path open/rdwr))
  (set-uart-config fd speed parity)
  fd)

(define uart-close file-close)
)
