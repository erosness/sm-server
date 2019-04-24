;;; cube-server bluetooth:

(use irregex matchable nanoif)
(import restlib turi
        (only incubator alist-merge)
        (only rest-player *pq* bt-notification)
        (only playqueue pq-current)
        (only rest-player player-information /v1/player/current))
(import bt-player)

;; ==================== BT NOTIFIER ====================
;;
;; We get notifications over nanaomessage from the bluetooth driver
;; (drv_bluetooth). See TS1003 for specification.

;; Some static variables keeping track of the state of the Bluetooth
;; connection as reported by the driver.
(define bt-title #f)
(define bt-subtitle #f)
(define bt-notifier-ar 44100)
(define bt-paused? #t)
(define bt-pairing? #f)
(define bt-device #f)


(define-local-turi-adapter bluetooth-turi "bt"
  (lambda (params)
    (print "BT turi-adapter ar=" bt-notifier-ar)
    `((url . "default:CARD=imxaudiobtm720")
      ,@(if bt-notifier-ar `((ar . ,bt-notifier-ar)) `()))))

;; ======================= Bluetooth REST interface ====================
;;

(define-handler /v1/catalog/bt
  (lambda ()
    `((turi . , (bluetooth-turi '()))
      (title . "Bluetooth")
      (type . "bt"))))

(define-handler /v1/catalog/bt/pair
  (lambda ()
    (if (current-json)
      (let* ((json-request (current-json))
        (pair?-cmd (alist-ref 'pair? json-request)))
          (set! bt-pairing? pair?-cmd)
          (if pair?-cmd
            (print "Start pairing!") ;; TODO: Fill inimplemetations
            (print "Stop pairing!"))
          `((status . "Ok")))
      `((pairing? . ,bt-pairing? )))))

(define (restart-cplay/bluetooth!)
  (parameterize ((current-json (/v1/catalog/bt)))
    (/v1/player/current)))


;; Process incoming updates from bluetooth driver.

(define (update-current-meta payload)
  (print "Update current meta=" (player-information) " Payload=" payload)
    (let ((from-bt-title (or (alist-ref 'title payload) "(no title)"))
          (from-bt-subtitle (or (alist-ref 'subtitle payload) "(no artist)")))
      (set! bt-subtitle (string-concatenate (list from-bt-title " - " from-bt-subtitle)))
      (let ((msg  `((subtitle . ,bt-subtitle))))
        (bt-notification msg))
      (let ((msg  `((subtitle . ,bt-subtitle)(title . ,bt-title))))
        (send-notification "/v1/player/current" msg))))

(define (connection-text connection device)
  (match connection
    (0 "Disconnected")
    (1 device )
    (2 "Pairing")
    (else "")))

(define (update-current-status payload)
  (if (equal? "bt" (alist-ref 'type (player-information)))
    (let* ((connect-status (alist-ref 'connection payload))
           (device (alist-ref 'player payload))
           (title `((title . ,(connection-text connect-status device))))
           (msg `((title . ,title))))
      (set! bt-title title)
      (bt-notification msg))
    (let ((msg  `((subtitle . ,bt-subtitle)(title . ,bt-title))))
      (send-notification "/v1/player/current" msg))))

(define (bt-handler obj)
  (let ((main-key ( car ( car obj))))
    (match main-key
      ('metadata
        (and-let* ((payload (alist-ref 'metadata obj)))
          (let* ((source-sets-paused (alist-ref 'paused payload))
                (current-is-bt (equal? "bt" (alist-ref 'type (player-information)))))

            (print "Payload=" payload
                    " source-sets-paused=" source-sets-paused
                    " bt-paused?=" bt-paused?
                    " current-is-bt:" current-is-bt )
              (if (and bt-paused? (not source-sets-paused) current-is-bt)
                (restart-cplay/bluetooth!))
              (set! bt-paused? source-sets-paused)
              (update-current-meta payload))))
      ('status
        (and-let* ((payload (alist-ref 'status obj)))
          (update-current-status payload)))
      (else (print "At else")))
    (print "leaving")))



(bt-set-handler bt-handler)

;;(begin
;;  (handle-exceptions e (void) (thread-terminate! bt-notifier))
;;  (define bt-notifier
;;    (thread-start!
;;     (->> (lambda () (bt-notifier-iteration))
;;          (loop/interval 1)
;;          (loop/exceptions (lambda (e) (pp `(error: ,(current-thread)
;;                                               ,(condition->list e))) #t))
;;          (loop)))))
