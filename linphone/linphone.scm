(module linphone *

(import chicken scheme foreign)
(use posix)

#>
#include <linphone/linphonecore.h>
#include "wrap-linphone.c"
<#

(define linphone-berit
  (foreign-lambda* int ((int a) (int b)) "return (berit(a,b));"))

;; integer typed variant of ioctl (as opposed to the ioctl egg's
;; pointers)
(define linphone_core_new
  (foreign-lambda* c-pointer ((c-pointer fd) (c-pointer command) (c-pointer argx) (c-pointer argy))
                   "return(linphone_core_new(fd, command, argx, argy));"))

(define (i2c-set-slave-addr fd i2c-addr)
  (linphone_core_new fd 5 i2c-addr))

(define (i2c-open path #!optional (i2c-addr #x34))
  (define fd (file-open path open/rdwr))
  (let ((ret (i2c-set-slave-addr fd i2c-addr)))
    (if (< ret 0)
        (error "i2c-set-slave-addr: Failed to acquire bus access and/or talk to slave. Got " ret)))
  fd)

(define i2c-close file-close))
