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

(import restlib turi
        (only rest-player *pq*)
        (only playqueue pq-current)
        (only rest-player player-information))

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
(define (IND-decompose line)
  ;; check for prefix and return the rest of the string if match
  (define (prefix str) (and (string-prefix? str line)
                            (string-drop line (string-length str))))
  (define ((labeler key) value) (list key value))
  (cond ((prefix "IND:-A1") => (labeler 'song))
        ((prefix "IND:-A2") => (labeler 'artist))
        ((prefix "IND:-A3") => (labeler 'album))))

;; aggregated bt-notifier state
(define bt-notifier-artist #f)
(define bt-notifier-album #f)
(define bt-notifier-song #f)

;; update bt-notifier state
;; (IND-process! "IND:-A1PRefs Paradise")
(define (IND-process! line)
  (match (IND-decompose line)
    (('song name)   (set! bt-notifier-song name))
    (('artist name) (set! bt-notifier-artist name))
    (('album name)  (set! bt-notifier-album name))
    (else #f)))

;; use bt-notifier-* state and broadcast to clients
(define (notify!)
  (let ((msg (alist-merge (player-information)
                          `((title    . ,(or bt-notifier-album "Bluetooth"))
                            (subtitle . ,(or bt-notifier-song ""))))))
    (send-notification "/v1/player/current" msg)))


(import process-cli) ;; TODO: dependency-graph is getting messy
(define bt-port (open-input-file*/nonblock (file-open "/dev/ttymxc3" open/read)))

(define (bt-notifier-iteration)
  (let ((line (read-line bt-port)))

    (display (conc "bt-notifier: line "
                   (with-output-to-string (cut write line))
                   "\n")
             (current-error-port))

    (IND-process! line) ;; update global vars

    (if (equal? "bt" (alist-ref 'type (or (pq-current *pq*) '())))
        (notify!))))

(begin
  (handle-exceptions e (void) (thread-terminate! bt-notifier))
  (define bt-notifier
    (thread-start!
     (->> (lambda () (bt-notifier-iteration))
          (loop/interval 1)
          (loop/exceptions (lambda (e) (pp `(error: ,(current-thread)
                                               ,(condition->list e))) #t))
          (loop)))))

;; ======================= Bluetooth REST interface ====================
;;
;; this snippet could have been in rest-alsa-capture.scm with the
;; others, but it needs to know the sampling-rate which we get from
;; sniffing the BTM720's UART interface (see IND:-S)

(define-turi-adapter bluetooth-turi "bt"
  (lambda (params)
    `((url . "default:CARD=imxaudiobtm720")
      (format . "alsa")
      (ar . "44100"))))

(define-handler /v1/catalog/bt
  (lambda ()
    `((turi . , (bluetooth-turi '()))
      (title . "Bluetooth")
      (type . "bt")
      (image . "http://www.kirya.net/wp-content/uploads/2007/07/bluetooth_logo.png"))))
