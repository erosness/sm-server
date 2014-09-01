;;; simple interface to amixer
;;; set/get volume and EQ using the command-line (eg. `amixer sget Master playback 20%`)


(module amixer *

(import chicken scheme extras)
(use posix test data-structures irregex)


;; parse simple-control get output
(define (amixer-parse/sget str)
  (cond ((irregex-search '(: "Front Left: Playback" (* space) (+ num) ;; internal volume
                             (* space)
                             "[" (=> L% (+ num)) "%]" (* space)
                             (=> L-mute (or "[on]" "[off]"))
                             ;;"Front Right: Playback " (* space) (+ num) (* space)
                             ;;"[" (=> %r (+ num)) "%]"
                             )
                         str) =>
                         (lambda (m) (string->number (irregex-match-substring m 'L%))))
        (else (error "could not parse amixer output" str))))

(test-group
 "amixer-parser/sset"
 (test
  "left channel"
  20
  (amixer-parse/sget "Simple mixer control 'Master',0
  Capabilities: pvolume pswitch pswitch-joined
  Playback channels: Front Left - Front Right
  Limits: Playback 0 - 65536
  Mono:
  Front Left: Playback 13108 [20%] [on]
  Front Right: Playback 13108 [20%] [on]
")))

(define (amixer-volume #!optional (value #f))

  (if (and value (not (number? value))) (error "invalid volume " value))

  (let ((output (if value
                    (with-input-from-pipe (conc "amixer sset Master Playback " value "%") read-string)
                    (with-input-from-pipe "amixer sget Master Playback" read-string))))

    (amixer-parse/sget output)))
)
