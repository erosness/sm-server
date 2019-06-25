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
(define rp-turi #f)
(define rp-subaddr #f)

(define-local-turi-adapter record-player-turi "record-player"
  (lambda (params)
    `((url . "??TBD??")
      ,@(if bt-ar `((ar . ,bt-ar)) `()))))

;; ======================= Bluetooth REST interface ====================
;;

(define-handler /v1/catalog/record-player
  (lambda ()
    (if rp-turi
    `((turi .     ,rp-turi)
      (title .    "IXION Symphony")
      (subtitle . "Record Player")
      (type .     "record-player"))
    (response-unavailable))))

;; ======================= Detect sources ==============================

(use medea http-client clojurian-syntax looper)
(define (*fetch-source-turi subaddr)
  (let ((addr rp-subaddr)
        (turi rp-turi)
        (current (fetch-source-turi subaddr)))
    (if (and turi (not current) addr (= subaddr addr))
      (begin
        (set! rp-turi #f)
        (set! rp-subaddr #f)))
    (if (and current (not turi))
      (begin
        (set! rp-turi current)
        (set! rp-subaddr subaddr)))))

(define (fetch-source-turi subaddr)
  (let ((http-request (conc "http://192.168.42." (number->string subaddr) ":5055/v1/source/icon")))
    (handle-exceptions x #f
      (receive (payload-string uri response)
        (with-input-from-request http-request #f read-string)
        (alist-ref 'turi (read-json payload-string))))))

;; Set up delayed refresh
(define (spawn-source-detect-threads)
  (print "Detect:" rp-subaddr " = " rp-turi)
  (do ((subaddr 2 (+ subaddr 1))) ((= subaddr 15))
    (thread-start!
      (make-thread
        (lambda ()
          (*fetch-source-turi subaddr))))))

(define (make-source-detect-thread)
  (thread-start!
    (->>
      spawn-source-detect-threads
      (loop/interval 20)
      (loop)
      ((flip make-thread) "Source detect thread"))))
