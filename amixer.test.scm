
;;; ==================== dead code ====================
;;;
;;; this can make amixer work on your laptop. using simple controls
;;; instead of cset/cget which is more abstract and nicer.


(test-group
 "amixer-parser/sget"
 (test
  "left channel"
  '(20)
  (amixer-parse/sget "Simple mixer control 'Master',0
  Capabilities: pvolume pswitch pswitch-joined
  Playback channels: Front Left - Front Right
  Limits: Playback 0 - 65536
  Mono:
  Front Left: Playback 13108 [20%] [on]
  Front Right: Playback 13108 [20%] [on]
"))
 ;; actual output
 (test
  "simple adb shell alsa_amixer"
  '(25)
  (amixer-parse/sget
   "Simple mixer control 'Master',0
  Capabilities: volume volume-joined cswitch cswitch-joined penum
  Playback channels: Mono
  Capture channels: Mono
  Limits: 0 - 100
  Mono: 21 [25%] Capture [off]
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
"))


 (test "negative values"
       '(-2000)
       (amixer-parse/values "numid=4,iface=MIXER,name='Tone Control 0 Gain'
  ; type=INTEGER,access=rw------,values=1,min=-2000,max=2000,step=1
  : values=-2000\n")))


(test-group
 "cmd/getter-setter"
  (define cmd (cmd/getter-setter "amixer" "cget" "cset" "Master"))
  (test "amixer cget Master" (cmd))
  (test "amixer cset Master 1,2,3" (cmd '(1 2 3))))
