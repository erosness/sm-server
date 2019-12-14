(module linphone *

(import chicken scheme foreign srfi-18 clojurian-syntax data-structures)
(use looper posix)

#>
#include <linphone/linphonecore.h>
#include "wrap-linphone.c"
<#

;; top-levels used in this module
(define lc #f)

;; Interface to wrapper
(define lphw-berit
  (foreign-lambda* int ((int a) (int b))
    "return (berit(a,b));"))

(define lphw-create
  (foreign-lambda* c-pointer ()
    "return (lph_create());"))

;; Interface to liblinphone
(define lphl-core-iterate
  (foreign-lambda* void ((c-pointer lc))
    "linphone_core_iterate(lc);"))

;; Iterator pacing libphone
(define (lph-iterate-body)
  (if lc (lphl-core-iterate lc)))

(define connect-button-thread
  (thread-start!
    (->>
      lph-iterate-body
      (loop/interval 0.05)
      (loop)
      ((flip make-thread) "Linphone core iterate thread"))))

;; Calls
(define (lph-create)
  (set! lc (lphw-create)))

(define lph-call
  (foreign-lambda* int () "return (lph_create());"))


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
