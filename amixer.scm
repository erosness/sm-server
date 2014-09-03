;;; simple interface to amixer
;;; set/get volume and EQ using the command-line (eg. `amixer sget Master playback 20%`)


(module amixer *

(import chicken scheme extras)
(use posix test data-structures irregex biquad srfi-1 srfi-13)

;; parse the output of amixer cget and cget commands.
(define (amixer-parse/values str)

  (let ((regex '(: ":" (* space) "values=" (=> vals (+ (+ num) (or "," "\n"))))))

    (cond ((irregex-search regex str) =>
           (lambda (m)
             (map string->number
                  (string-split
                   (irregex-match-substring m 'vals)
                   ",\n"))))
          (else (error "could not parse" str)))))



;; helper for constructing amixer getter and setter command-line
;; strings. returns a procedure which returns the command to use: get
;; if vals if false or set if vals is true with vals appended at the
;; end.
(define (cmd/getter-setter prefix get set postfix)

  (define (eq->string vals) (string-join (map number->string vals) ","))

  (lambda (#!optional vals)
    (if vals
        (conc prefix " " set " " postfix " "  (eq->string vals))
        (conc prefix " " get " " postfix))))


;; construct a proc which can get and set using the
;; command-line-strings given by cmd-proc. everything is parsed with
;; amixer-parse/values
(define (make-amixer-getter/setter cmd-proc)
  (lambda (#!optional vals)
    (assert (or (eq? #f vals) (and (list? vals))))

    (amixer-parse/values
     (with-input-from-pipe (if vals
                               (cmd-proc vals)
                               (cmd-proc))
                           read-string))))

(define amixer-volume/cube
  (make-amixer-getter/setter
   (cmd/getter-setter "alsa_amixer -c 1" "cget" "cset" "name=\"Playback Volume (plain)\"")))


(define amixer-volume amixer-volume/cube)

(include "amixer.test.scm")
)
