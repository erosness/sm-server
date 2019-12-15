(module linphone *

(import chicken scheme foreign srfi-18 clojurian-syntax data-structures)
(use looper posix)

#>
#include <linphone/linphonecore.h>
#include "wrap-linphone.c"
<#

;; top-levels used in this module
(define lc #f)
(define cnt1 0)

;; Interface to wrapper
(define lphw-create
  (foreign-lambda* c-pointer ()
    "return (lph_create());"))

(define lphw-core-destroy
  (foreign-lambda* void ((c-pointer core))
    "linphone_core_destroy(core);"))

(define lphw-call
  (foreign-lambda* c-pointer () "return (lph_create());"))

;; Interface to liblinphone
(define lphl-core-iterate
  (foreign-lambda* void ((c-pointer lc))
    "linphone_core_iterate(lc);"))

;; Iterator pacing libphone
(define (lph-iterate-body)
  (if lc (begin
    (lphl-core-iterate lc)
    (set! cnt1 (+ 1 cnt1)))))

(define connect-button-thread
  (lambda ()
    (thread-start!
      (->>
        lph-iterate-body
        (loop/interval 0.05)
        (loop)
        ((flip make-thread) "Linphone core iterate thread")))))

;; Calls
(define (lph-create)
  ;; Start iterator
  (connect-button-thread)
  ;; Create phone
  (set! lc (lphw-create)))


;; Callback test
(define (cb-test n)
  (print "At callback test" n)
  (* n n))

(define-external (cb_itself (int y)) int (cb-test y))

(define berit-cprog
  (foreign-safe-lambda* int ((int a)(int b))
    "return(berit(a , b));"))


)
