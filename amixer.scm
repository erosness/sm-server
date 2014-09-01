;;; simple interface to amixer
;;; set/get volume and EQ using the command-line (eg. `amixer sget Master playback 20%`)


(module amixer *

(import chicken scheme extras)
(use posix test data-structures irregex)

;; parse the output of amixer cget and cset commands.
(define (amixer-parse/cget str)

  (let ((regex '(: ":" (* space) "values=" (=> left (+ numeric)) )))

    (cond ((irregex-search regex str) =>
           (lambda (m)
             (define (n->num label) (string->number (irregex-match-substring m label)))
             (n->num 'left)))
          (else (error "could not parse" str)))))

(test-group
 "amixer-parse/cget"

 (test "simplest amixer case" '0 (amixer-parse/cget "\n  : values=0,0"))

 (test "amixer result parser"
       10
       (amixer-parse/cget
        "numid=3,iface=MIXER,name='Master Playback Volume'
  ; type=INTEGER,access=rw------,values=2,min=0,max=65536,step=1
  : values=10,10\n"))


 (test "alsa_amixer android"
       13
       (amixer-parse/cset
        "numid=110,iface=MIXER,name='Playback Volume (plain)'
  ; type=INTEGER,access=rw---R--,values=1,min=0,max=8388608,step=0
  : values=13
  | dBscale-min=-80.00dB,step=0.80dB,mute=0
")))


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



;; TODO: deprecate this once simple controls become available on Cube.
;; we want to use the same API for both.
(define (amixer-volume/cube #!optional value)

  (let* ((scale (/ 65500 100))
         (output (if value
                     ;; set volume
                     (with-input-from-pipe
                      (conc "alsa_amixer -c 1 cset name=\"Playback Volume (plain)\" "
                            (* value scale))
                      read-string)
                     ;; get volume
                     (with-input-from-pipe
                      "alsa_amixer -c 1 cget name=\"Playback Volume (plain)\" "
                      read-string))))

    (/ (amixer-parse/cget output) scale)))


(define (amixer-volume/simple #!optional (value #f))

  (if (and value (not (number? value))) (error "invalid volume " value))

  (let ((output (if value
                    (with-input-from-pipe (conc "amixer sset Master Playback " value "%") read-string)
                    (with-input-from-pipe "amixer sget Master Playback" read-string))))

    (amixer-parse/sget output)))

(define amixer-volume amixer-volume/cube)

)
