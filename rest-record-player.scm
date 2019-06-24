;;; cube-server record-player:

(use irregex matchable nanoif)
(import restlib turi
        (only incubator alist-merge)
        (only rest-player *pq* bt-notification)
        (only playqueue pq-current)
        (only rest-player player-information /v1/player/current))
(import bt-player)

;; Some static variables keeping track of the state of the Bluetooth
;; connection as reported by the driver.
(define rc-turi "http://turi----TBD")
(define rc-title "Bluetooth")
(define rc-subtitle "Sang X")

(define-local-turi-adapter record-player-turi "record-player"
  (lambda (params)
    `((url . "??TBD??")
      ,@(if bt-ar `((ar . ,bt-ar)) `()))))

;; ======================= Bluetooth REST interface ====================
;;

(define-handler /v1/catalog/record-player
  (lambda ()
    `((turi . , (bluetooth-turi '()))
      (title . ,bt-title)
      (subtitle . ,bt-subtitle)
      (type . "bt"))))

(define (fetch-source-icon)
  (print "Fetch..."))

;; Set up delayed refresh
(thread-start!
  (make-thread
    (lambda ()
      (thread-sleep! 5)
      (fetch-source-icon))))
