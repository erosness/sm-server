;;; cube-server bluetooth:
;;;
;;; because things are getting a little more complex. the BT-notifier
;;; thread will read from the BTM720's UART on ttymxc3. Here, the BT
;;; unit will tell us the sampling rate of the incoming data
;;; (IND:-S48000Hz). This sampling-rate needs to be passed to cplay on
;;; the command-line in order for things to work (cplay can't detect
;;; this on its own :(). For that reason, the turi-adapter and the
;;; bt-notifier need to communicate and probably are best grouped
;;; together in this file.
;;;
;;; - TODO: bt-notifier must set sampling-rate as well as song title etc
;;; - TODO: bluetooth turi adapter must construct cplay command with sampling-rate
;;; - TODO: restart cplay when Bluetooth sampling rates changes
;;;

(use irregex matchable nanoif)
(import restlib turi
        (only incubator alist-merge)
        (only rest-player *pq*)
        (only playqueue pq-current)
        (only rest-player player-information /v1/player/current))
(import bt-player)

;; ==================== BT NOTIFIER ====================
;;
;; we get one line per item-notification from the BT agent. it
;; typically send this across the UART:
;;
;; IND:-A1Happiness
;; IND:-A2Jónsi & Alex
;; IND:-A3Riceboy Sleeps
;; IND:-A7561000ms
;;
;; these are our BT UART assumptions:
;;
;; 1. we don't know that the order is the same
;; 2. we don't know the timings of each line
;; 3. we don't expect a lot of lines coming in from the BT module
;;
;; wanting to be as robust as possible, we read line by line and pick
;; up song/artist/album into a state (like bt-notifier-artist). on
;; each line we update our state, and send the aggregated state to our
;; client because:
;;
;; the clients have a limitation: sending notifications with only
;; title and no subtitle, for example, will clear the subtitle field
;; in the display - so we can't send title alone.
;;
;; step 3 above means it should be safe to send one notify! on each
;; line (there are few of them and only when user changes
;; song/connects/disconnects.
;;
;; we also merge in the current (player-information) in the
;; notification, this prevents us from losing the static fields, most
;; noticeably 'image' and 'type' in the ui.
;;
;; parse lines like:
;; (IND-decompose "IND:-A1The Ludlows")
;; (IND-decompose "IND:-A2James Horner")
;; (IND-decompose "IND:-A3Legends Of The Fall Original Motion Picture Soundtrack")
;; (IND-decompose "IND:-S44123Hz")
;; (IND-decompose "IND:-M0")
;; (IND-decompose "IND:-M1")
(define (IND-decompose line)
  ;; check for prefix and return the rest of the string if match
  (define (prefix str) (and (string-prefix? str line)
                            (string-drop line (string-length str))))
  (define ((labeler key) value) (list key value))
  (cond ((prefix "IND:-A1") => (labeler 'song))
        ((prefix "IND:-A2") => (labeler 'artist))
        ((prefix "IND:-A3") => (labeler 'album))
        ((equal? line "IND:-M1") 'mute)
        ((equal? line "IND:-M0") 'unmute)
        ((equal? line "IND:-C1") 'connect)
        ((equal? line "IND:-C0") 'disconnect)
        ((irregex-match `(: "IND:-S" (=> hz (* digit)) (w/nocase "Hz")) line) =>
         (lambda (match) `(ar ,(string->number (irregex-match-substring match 'hz)))))))

;; aggregated bt-notifier state
(define bt-notifier-artist #f)
(define bt-notifier-album #f)
(define bt-notifier-song #f)
(define bt-notifier-ar 44100)
(define bt-paused? #t)
(define bt-pairing? #f)
(define bt-notifier-device #f)

;; ======================= Bluetooth REST interface ====================
;;
;; this snippet could have been in rest-alsa-capture.scm with the
;; others, but it needs to know the sampling-rate which we get from
;; sniffing the BTM720's UART interface (see IND:-S)

(define-local-turi-adapter bluetooth-turi "bt"
  (lambda (params)
    (print "BT turi-adapter ar=" bt-notifier-ar)
    `((url . "default:CARD=imxaudiobtm720")
      ,@(if bt-notifier-ar `((ar . ,bt-notifier-ar)) `()))))

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
            (print "Start pairing!")
            (print "Stop pairing!"))
          `((status . "Ok")))
      `((pairing? . ,bt-pairing? )))))

(define (restart-cplay/bluetooth!)
  (parameterize ((current-json (/v1/catalog/bt)))
    (/v1/player/current)))

;; ======================
;; typical IND sequence:
;; (IND-decompose "IND:*C1") ;; bt connected
;; (IND-decompose "IND:-C2") ;; bt codec (1:SBC, 2=AAC, 3=aptX)
;; (IND-decompose "IND:-S44100Hz") ;; set samplerate
;; (IND-decompose "IND:-M0") ;; <-- will start cplay at 44100hz
;; (IND-decompose "IND:-A1The Lonely Mountains")
;; (IND-decompose "IND:-A2Kim Janssen")
;; (IND-decompose "IND:-A3The Lonely Mountains")
;;
;; note that song/artist etc comes after unmute, so although
;; (/v1/catalog/bt) just fills a dummy title, it should be fixes by
;; the subsequent A1-A3's.

;; update bt-notifier state
;; (IND-process! "IND:-A1PRefs Paradise")
(define (IND-process! line)
  (match (IND-decompose line)
    (('song name)   (set! bt-notifier-song name))
    (('artist name) (set! bt-notifier-artist name))
    (('album name)  (set! bt-notifier-album name))
    (('ar ar)       (set! bt-notifier-ar ar))
    (('connect)     (print "Connect"))
    (('disconnect)  (print "Disconnect"))
;;    ('unmute        (restart-cplay/bluetooth!))
    (else #f)))

;; use bt-notifier-* state and broadcast

(define (notify!)
  (let ((msg (alist-merge (player-information)
                          `((title    . ,(or bt-notifier-album "Bluetooth"))
                            (subtitle . ,(or bt-notifier-song ""))
                            (btdevice . ,(or bt-notifier-device "Disconnected"))))))
    (send-notification "/v1/player/current" msg)))


;;(import process-cli)
 ;; TODO: dependency-graph is getting messy
;;(define bt-port (open-input-file*/nonblock (file-open "/dev/ttymxc3" open/read)))

;;(define (bt-notifier-iteration)
;;  (let ((line (read-line bt-port)))

;;    (display (conc "bt-notifier: line "
;;                   (with-output-to-string (cut write line))
;;                   "\n")
;;             (current-error-port))))
;; Temporary - keep printout for debug, do not update current.
;;    (IND-process! line) ;; update global vars

;;    (if (equal? "bt" (alist-ref 'type (or (pq-current *pq*) '())))
;;        (notify!))))

;; Set handler to redirect BT event here.

(define (xor a b) (or (and a (not b))(and (not a) b)))

(define (update-current-meta payload)
  (let ((current (pq-current *pq*)))
    (print "Update current Current=" current " Payload=" payload)
    (if (equal? "bt" (alist-ref 'type current))
      (alist-update 'pause bt-paused? current))
    ))

(define (bt-handler obj)
  (let ((main-key ( car ( car obj))))
    (print "bt-handler-obj: Main key=" main-key)
    (match main-key
      ('metadata
        (print "In match - metadata")
        (and-let* ((payload (alist-ref 'metadata obj)))
          (let* ((source-sets-paused (alist-ref 'paused payload))
                (current (or (pq-current *pq*) '()))
                (current-is-bt (equal? "bt" (alist-ref 'type current))))

            (print "Payload=" payload
                    " source-sets-paused=" source-sets-paused
                    " bt-paused?=" bt-paused?
                    " current-is-bt:" current-is-bt )
              (if (and bt-paused? (not source-sets-paused) current-is-bt)
                (restart-cplay/bluetooth!))
              (set! bt-paused? source-sets-paused)
              (update-current-meta payload)
              (if current-is-bt
                (send-notification "/v1/player/current" payload)))))
      ('status'
        (print "In match - status" obj))
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
