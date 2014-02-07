#!/bin/csi -s
;;; sine wave tone generator.
;;; output sample-format is u8
(use clojurian-syntax bitstring extras)

(if (not (= 3 (length (command-line-arguments))))
    (error (conc "usage: tone-generator <freq hz> <amplitude> <duration>\n"
                 "\noutput is always pcm s16 little endian\ntry this: tone-generator 440 20000 1 | aplay -r 44100 -f s16_le -c 2")))

(define π (* 4 (atan 1)))

(define (tones freq amplitude #!optional (duration 1) (proc display))
  (let ((sr     44100)) ;; hz, samplerate
    (let loop ((x 0))
      (when (or #f (< x (* sr duration)))
        (proc (-> (sin (/ (* freq 2 π x) sr))
                ;;(+ 1)         ;; we're doing unsigned
                (* amplitude) ;; not too loud
                (floor)
                (inexact->exact)))
        (loop (add1 x))))))

;; (tones 440 40 0.01 (lambda (x) (print (make-string x #\#))))
(tones (string->number (car (command-line-arguments)))
       (string->number (cadr (command-line-arguments)))
       (string->number (caddr (command-line-arguments)))
       (lambda (x)
         (let ((sample (->> (bitconstruct (x 16 little))
                            (bitstring->blob)
                            (blob->string))))
           (display sample) ;; right
           (display sample) ;; left
           )))


