
;;; ==================== dead code ====================
;;;
;;; this can make amixer work on your laptop. using simple controls
;;; instead of cset/cget which is more abstract and nicer.

;; parse simple-control get output. it's different from values. values
;; seems to be general, where sget gives you percentages and
;; left/right explicitly. values
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


(define (amixer-volume/simple #!optional (value #f))

  (if (and value (not (number? value))) (error "invalid volume " value))

  (let ((output (if value
                    (with-input-from-pipe (conc "amixer sset Master Playback " value "%") read-string)
                    (with-input-from-pipe "amixer sget Master Playback" read-string))))

    (amixer-parse/sget output)))

(test-group
 "amixer-parser/sget"
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



;;; ==================== tests ====================

(test-group
 "amixer-parse/values"

 (test "simplest amixer case" '(1 2) (amixer-parse/values "\n  : values=1,2\n"))

 (test "amixer result parser"
       '(11 22)
       (amixer-parse/values
        "numid=3,iface=MIXER,name='Master Playback Volume'
  ; type=INTEGER,access=rw------,values=2,min=0,max=65536,step=1
  : values=11,22\n"))


 (test "alsa_amixer android"
       '(13)
       (amixer-parse/values
        "numid=110,iface=MIXER,name='Playback Volume (plain)'
  ; type=INTEGER,access=rw---R--,values=1,min=0,max=8388608,step=0
  : values=13
  | dBscale-min=-80.00dB,step=0.80dB,mute=0
")))


(test-group
 "cmd/getter-setter"
  (define cmd (cmd/getter-setter "amixer" "cget" "cset" "Master"))
  (test "amixer cget Master" (cmd))
  (test "amixer cset Master 1,2,3" (cmd '(1 2 3))))
