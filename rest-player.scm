(module rest-player ()

(import chicken scheme data-structures)

(import rest player)

(define-handler /pause
  (lambda () (player-pause)))

(define-handler /unpause
  (lambda () (player-unpause)))

)
