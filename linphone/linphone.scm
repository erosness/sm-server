(module linphone (  lph-create-caller
                    lph-create-answerer
                    lph-call
                    lph-terminate
                    lph-status)

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
(define current-state "idle")
(define state-callback (lambda(lc call state msg) (print "At state-callback")))

(define (ip->sip ip)
  (string-append "sip:pi@" ip))

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

(define lphl-accept
  (foreign-lambda* int ((c-pointer lc) (c-pointer call))
    "return(linphone_core_accept_call(lc, call));"))

(define lphl-call-get-state
  (foreign-lambda* int ((c-pointer call))
    "return(linphone_call_get_state(call));"))

(define lphl-terminate-all-calls
  (foreign-safe-lambda* int ((c-pointer lc))
    "return(linphone_core_terminate_all_calls(lc));"))

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
(define (lph-status)
  `(( connection . "none")
    ( state . ,current-state)))

(define status-caller
  (lambda()
    `(( connection . "caller")
      ( state . ,current-state))))

(define status-answerer
  (lambda()
    `(( connection . "answerer")
      ( state . ,current-state))))


(define (lph-create-caller)
  (set! lph-status status-caller)
  (set! state-callback caller-state-callback)
  ;; Start iterator
  (core-iterate-thread)
  ;; Create phone
  (set! lc (lphw-create)))

(define (lph-create-answerer)
  (set! lph-status status-answerer)
  (set! state-callback answerer-state-callback)
  ;; Start iterator
  (core-iterate-thread)
  ;; Create phone
  (set! lc (lphw-create)))

(define (lph-call dest)
  (if lc
    (begin
      (set! call (lphw-call lc (ip->sip dest)))
      (set! current-state "connecting"))))

(define (lph-answer)
  (lphl-accept lc call))

(define (lph-terminate)
  (lphl-terminate-all-calls lc))

;; Callback call state
(define caller-state-callback
  (lambda (core call cstate msg)
  (match cstate
    ( 7 (set! current-state "connected"))
    (13 (set! current-state "idle"))
    (18 (set! current-state "idle"))
    (else (print "Case not handled caller:" cstate)))))

(define answerer-state-callback
  (lambda (core cb-call cstate msg)
  (match cstate
    ( 1 (begin
          (set! call cb-call)
          (set! current-state "connecting")
          (lph-answer)))
    ( 7 (set! current-state "connected"))
    (13 (set! current-state "idle"))
    (18 (set! current-state "idle"))
    (else (print "Case not handled answerer:" cstate)))))

(define-external
  (state_changed (c-pointer core)(c-pointer call)(int state)(c-string msg))
  void
  (state-callback core call state msg))

)
