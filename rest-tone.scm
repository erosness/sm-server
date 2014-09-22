(module rest-tone ()

(import chicken scheme data-structures)

(use uri-common restlib test srfi-1)
(import turi)


(define (play-command/tone hz)
  `((format . "lavfi")
    (url    . ,(conc "aevalsrc=sin(" hz "*2*PI*t):s=8000:d=1"))))

(define-turi-adapter tone->turi "tone" play-command/tone)
;; (tone->turi 440) ==> "tr://localhost:5060/t2s?type=tone&id=440"

(with-request ("" `((host ("foo" . 10))))
              (tone->turi '((id . 100))))

(define (tones)
  (map (lambda (hz)
         (let ((hz (* hz 100)))
           `((title . ,(conc hz " hertz"))
             (duration . 1)
             (turi . ,(tone->turi hz)))))
       (map add1 (iota 100))))

(define-handler /v1/catalog/tone (pagize (argumentize (lambda (q) ((querify (tones)) q)) 'q)))

)

;; this is noisy:
;; (play! (play-command "tr://localhost:5060/t2s?type=tone&id=440"))
;; (player-quit)
