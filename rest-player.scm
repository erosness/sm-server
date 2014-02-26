(define (/pause)
  (player-pause))

(define (/unpause)
  (player-unpause))

(define-handler "/pause" /pause)
(define-handler "/unpause" /unpause)
