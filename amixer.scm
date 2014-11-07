;;; simple interface to amixer
;;; set/get volume and EQ using the command-line (eg. `amixer sget Master playback 20%`)


(module amixer *

(import chicken scheme extras)
(use posix test data-structures irregex biquad srfi-1 srfi-13 srfi-18
     matchable looper clojurian-syntax)

(import process-cli notify)

;; parse the output of amixer cget and cget commands.
(define (amixer-parse/values str)

  (let ((regex '(: ":" (* space) "values="
                   (=> vals (+             ;; multiple values
                             (? "-")       ;; negative number
                             (+ num)       ;; single value
                             (or "," "\n") ;; terminator
                             )))))

    (cond ((irregex-search regex str) =>
           (lambda (m)
             (map string->number
                  (string-split
                   (irregex-match-substring m 'vals)
                   ",\n"))))
          (else (error "could not parse" str)))))

;; parse simple-control get output. it's different from values. values
;; seems to be general, where sget gives you percentages and
;; left/right explicitly. values
(define (amixer-parse/sget str)
  (cond ((irregex-search '(: ":"
                             (* space) (or "" "Capture" "Playback")
                             (* space) (+ num)
                             (* space) "[" (=> L% (+ num)) "%]"  ;; percentage
                             (* space) (or "" "Capture" "Playback") ;; todo remove capture
                             (* space) (=> L-mute (or "[on]" "[off]"))
                             ;;"Front Right: Playback " (* space) (+ num) (* space)
                             ;;"[" (=> %r (+ num)) "%]"
                             )
                         str) =>
                         (lambda (m)
                           (list (string->number (irregex-match-substring m 'L%)))))
        (else ;; pp str too because error truncates it.
         (pp `(error amixer-parse/sget ,str))
         (error "could not parse amixer output" str) )))



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
(define (make-amixer-getter/setter cmd-proc parser)
  (lambda (#!optional vals)
    (assert (or (eq? #f vals) (and (list? vals))))

    (let ((cmdstring (if vals
                         (cmd-proc vals)
                         (cmd-proc))))

      (pp `(debug shell ,cmdstring))

      (parser
       (call-with-input-pipe cmdstring
                             (lambda (p)
                               ;; wait for i/o in case amixer is slow
                               ;; to respond.
                               (thread-wait-for-i/o! (port->fileno p))
                               (read-string 2048 p)))))))


;; fp => (fp fp fp fp fp)
(define (make-biquad-converter band #!optional (rate 48000) (slope 0.7))

  ;; (test 0.5 (%->factor 75 1))
  ;; (test -2.0 (%->factor 0))
  (define (%->factor x #!optional (n 10))
    (* (- x 50) #|[-50,50]|#
       0.01     #|[-0.5 , 0.5]|#
       2        #|[-1,1]|#
       n        #|[-n,n]|#))

  (lambda (gain%)
    (map (o inexact->exact round (cut * <> 8388608))
         (sigma-eq-coefficients  (%->factor gain%) band rate slope))))

(define (eq-band-frequency band-index)
  (alist-ref band-index
             '((0 .    63)
               (1 .   120)
               (2 .  500)
               (3 .  2000)
               (4 . 10000))))
;; (eq-band-frequency 2)


(define (make-amixer-eq band-index)
  (assert (number? band-index))

  ;; we don't have amixer getters for EQ Stages because we can only
  ;; get coefficients back. so we cache EQ values (should be fixed in
  ;; interface for kernel)
  ;; TODO: make thread-safe or replace with a real cmdline getter
  (define cached-value 50)

  (define amixer-setter
    (make-amixer-getter/setter
     (cmd/getter-setter "alsa_amixer -c 1" "cget" "cset" (conc "name=\"EQ Stage " band-index "\""))
     amixer-parse/values))

  (define gain->coefficients (make-biquad-converter (eq-band-frequency band-index)))

  (lambda (#!optional val)
    (if (and val (not (eq? cached-value val))) ;; set if present and changed
        (begin
          (set! cached-value val)
          (amixer-setter (gain->coefficients val))))
    cached-value))

(define amixer-eq/cube
  (let ()
    ;; allocate 5 getter/setters with cached EQ-values
    (define amixer-eqs (list-tabulate 5 (lambda (idx) (make-amixer-eq idx))))

    ;; (or #f (fp fp fp ...))
    (lambda (#!optional gains)
      (map (lambda (getter/setter-proc gain)
             (getter/setter-proc gain))
           amixer-eqs
           (or gains '(#f #f #f #f #f))))))

;; returns a procedure which:
;;
;; with no arguments, returns the last read value
;; with 1 argument, writes using writer
;;
;; should currently be thread-safe as file-write on port pop is
;; atomic, but this could be cleaned up.
(define (make-cmixer-interface reader #!optional
                               (writer display)
                               (cmds '("cmixer" "Master Volume")))

  (define last-read #f)

  (receive (pip pop pid)
      (spawn* (car cmds) (cdr cmds))

    (define (cleanup!)
      ;; obs: errors if process already exited
      (process-wait pid))

    (define reader-thunk
      (->> (lambda ()
             (set! last-read (reader pip))
             (pp `(info amixer ,(current-thread) read ,last-read))
             (if (eof-object? last-read) #f #t))
           (loop/exceptions (lambda (e) (pp `(error cmixer-thread ,(condition->list e))) #f))
           (loop)
           ((lambda (h)
              (lambda ()
                (h)
                (cleanup!))))))

    (thread-start! (make-thread reader-thunk "cmixer-reader"))

    (match-lambda*
     (( '#:quit) (process-signal pid))
     (( x ) (set! last-read (writer x pop))) ;; <-- TODO: this is not good
     (( ) last-read))))

(begin
  (handle-exceptions e (void) (amixer-volume #:quit))
  (define amixer-volume
    (let ()

      (define (notify volume)
        (send-notification "/v1/player/volume" `((value . ,volume))))

      (make-cmixer-interface
       ;; read line-by-line from port p and convert to numberical value
       (lambda (p)
         (let* ((line (read-line p 1024))
                (volume (string->number (string-trim line))))
           (cond ((eof-object? line) line)
                 (else
                  (notify volume)
                  volume))))

       ;; write number x to p with appending newline
       (lambda (x p)
         (write `("setting volume to" ,x))
         (notify x)
         (display x p)
         (display #\newline p)
         x)))))

;; (amixer-volume)
;; (amixer-volume 55)


(define amixer-eq amixer-eq/cube)

(include "amixer.test.scm")
)
