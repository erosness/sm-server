
(define tones
  (map (lambda (hz)
         (let ((hz (* hz 100)))
           `((title . ,(conc hz " hertz"))
             (duration . 1)
             (turi . ,(conc "tr://tone/" hz)))))
       (map add1 (iota 100))))

(define-handler "/search/tone" (pagize (argumentize (querify tones) 'q)))

(define (play-command/tone uri)
 (let ((hz (second (uri-path uri))))
   (conc "cplay -f lavfi aevalsrc=\"sin(" hz "*2*PI*t):s=8000\"")))

(define-audio-host "tone" play-command/tone)

(test "cplay -f lavfi aevalsrc=\"sin(1234*2*PI*t):s=8000\"" (play-command "tr://tone/1234"))

;; this is noisy:
;; (play! (play-command "tr://tone/440"))
;; (player-quit)
