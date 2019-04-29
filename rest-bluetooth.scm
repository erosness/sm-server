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
(define bt-title "Bluetooth")
(define bt-subtitle " ")
(define bt-notifier-ar 44100)
(define bt-paused? #t)
(define bt-device #f)
(define bt-pairing? #f)
(define bt-connected? #f)

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
      (title . ,bt-title)
      (subtitle . ,bt-subtitle)
      (type . "bt"))))

(define-handler /v1/catalog/bt/connect
  (lambda ()
    (if (current-json)
      (let* ((json-request (current-json))
        (pair?-cmd (alist-ref 'pair? json-request)))
          (set! bt-pairing? pair?-cmd)
          (if pair?-cmd
            (bt-start-pair)
            (bt-end-pair))
          `((status . "Ok")))
      `((pairing? . ,bt-pairing? )
        (connected? . ,bt-connected? )
        (device . ,bt-device )))))

(define (restart-cplay/bluetooth!)
  (parameterize ((current-json (/v1/catalog/bt)))
    (/v1/player/current)))


;; Process incoming updates from bluetooth driver.

(define (update-current-meta payload)
    (let ((from-bt-title (or (alist-ref 'title payload) "(no title)"))
          (from-bt-subtitle (or (alist-ref 'subtitle payload) "(no artist)")))
      (set! bt-subtitle (string-concatenate (list from-bt-title " - " from-bt-subtitle)))
      (let ((msg  `((subtitle . ,bt-subtitle)(paused . ,bt-paused?))))
        (bt-notification msg))
      (if (equal? "bt" (alist-ref 'type (player-information)))
        (let ((msg  `((subtitle . ,bt-subtitle)(title . ,bt-title)(paused . ,bt-paused?))))
          (send-notification "/v1/player/current" msg)))))

(define (connection-text connection device)
  (match connection
    (0 (set! bt-subtitle " ") (set! bt-paused? #t) "Disconnected")
    (1 device )
    (2 (set! bt-subtitle " ") (set! bt-paused? #t) "Pairing")
    (else "")))

(define (update-current-status payload)
    (let* ((connect-status (alist-ref 'connection payload))
           (device (alist-ref 'player payload))
           (title-text (connection-text connect-status device))
           (msg `((title . ,title-text))))
      (set! bt-title title-text)
      (set! bt-connected? (equal? connect-status 1))
      (set! bt-device (if bt-connected? device "No device"))
      (bt-notification msg))
    (if (equal? "bt" (alist-ref 'type (player-information)))
      (let ((msg  `((subtitle . ,bt-subtitle)(title . ,bt-title))))
          (send-notification "/v1/player/current" msg))))

(define (bt-handler obj)
  (let ((main-key ( car ( car obj)))
        (current-is-bt (equal? "bt" (alist-ref 'type (player-information)))))
    (match main-key
      ('metadata
        (let* ((payload (alist-ref 'metadata obj))
               (source-sets-paused (alist-ref 'paused payload)))
          (if (and bt-paused? (not source-sets-paused) current-is-bt)
            (restart-cplay/bluetooth!))
          (set! bt-paused? source-sets-paused)
          (update-current-meta payload)))
      ('status
        (and-let* ((payload (alist-ref 'status obj)))
          (update-current-status payload)))
      (else (print "At else")))
    (print "leaving")))

(bt-set-handler bt-handler)
(bt-refresh)

;;(begin
;;  (handle-exceptions e (void) (thread-terminate! bt-notifier))
;;  (define bt-notifier
;;    (thread-start!
;;     (->> (lambda () (bt-notifier-iteration))
;;          (loop/interval 1)
;;          (loop/exceptions (lambda (e) (pp `(error: ,(current-thread)
;;                                               ,(condition->list e))) #t))
;;          (loop)))))
