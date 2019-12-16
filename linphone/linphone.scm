(module linphone *

(import chicken scheme foreign srfi-18 clojurian-syntax data-structures
        matchable)
(use looper posix)

#>
#include <linphone/linphonecore.h>
#include "wrap-linphone.c"

LinphoneCore *lph_create();
void linphone_core_destroy(LinphoneCore* core);
void linphone_core_iterate(LinphoneCore* core);

<#

;; top-levels used in this module
(define lc #f)
(define cnt1 0)
(define call #f)

;; Interface to wrapper
(define lphw-create
  (foreign-lambda* c-pointer ()
    "return (lph_create());"))

(define lphw-core-destroy
  (foreign-lambda* void ((c-pointer core))
    "linphone_core_destroy(core);"))

(define lphw-call
  (foreign-safe-lambda* c-pointer ((c-pointer lc)(c-string dest))
    "return (lph_call(lc, dest));"))

;; Interface to liblinphone
(define lphl-core-iterate
  (foreign-safe-lambda* void ((c-pointer lc))
    "linphone_core_iterate(lc);"))

;; Iterator pacing libphone
(define (lph-iterate-body)
  (if lc (begin
    (lphl-core-iterate lc)
    (set! cnt1 (+ 1 cnt1)))))

(define core-iterate-thread
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
  (core-iterate-thread)
  ;; Create phone
  (set! lc (lphw-create)))


;; Callback call state
(define (lphw-state-changed core call cstate msg)
  (match cstate
    ( 1 (print "Case: incoming call"))
    (13 (print "Case: error"))
    (18 (print "Case: call terminated"))
    (else (print "Case not handled:" cstate))))

(define-external
  (state_changed (c-pointer core)(c-pointer call)(int state)(c-string msg))
  void
  (lphw-state-changed core call state msg))

)
