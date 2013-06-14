
(define dsp-volume-set! (lambda (v) #f))
(define dsp-mute-set!   (lambda (v) #f))

;; redefine dsp- functions from android impl
;; iff compiling for android
(let-syntax ((maybe-include
              (ir-macro-transformer
               (lambda _ (if (feature? 'android)
                        `(include-relative "dsp-android.scm")
                        `(void))))))
  (maybe-include))
