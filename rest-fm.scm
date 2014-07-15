(use test restlib notify clojurian-syntax)
(import rest)

(define (fm-send-audio! khz #!optional (duration 0.5))
  ;; send a sound snippet to cplay
  (with-output-to-pipe
   (conc "ffmpeg -loglevel quiet -f lavfi "
         " -i \"aevalsrc=sin(" khz "*2*PI*t):d=" duration "\" "
         " -f s8 udp://localhost:3000?listen")
   void))

;; (fm-send-audio! 1000)
;; (fm-send-audio! 440 7)

(define *freq* 440)

;; create a thread that keeps pumping audio data to
;; udp://localhost:3000. if a player isn't listening there, it'll be
;; ignored. it sounds horrible but it allows testing if your *freq* is
;; being manipulated properly, and mimics the upcoming HW's model
;; relatively well.
(begin
  (handle-exceptions e (void) (thread-terminate! sa-thread))
  (define sa-thread
    (thread-start!
     (lambda ()
       (let loop ()
         (fm-send-audio! *freq* 0.1)
         (thread-sleep! 0.2)
         (loop))))))


;; ==================== explicit frequency ====================

(define-turi-adapter fmfreq->turi "fm"
  (lambda (khz)
    `((url . "udp://localhost:3000")
      (format . "s8") ;; <-- see "-f s8"
      ;; if live is #t and the turi is the same as what cplay is
      ;; currently , we don't need to re-initialize cplay because it'd
      ;; be playing the very same sound. if live is #f, cplay still
      ;; needs to be re-initialized because we might want to play from
      ;; the start.
      (live . #t))))

(define-handler /v1/catalog/fm/browse
  (lambda () `((turi . ,(fmfreq->turi "ignored")))))

(define-handler /v1/catalog/fm/seek
  (argumentize (lambda (khz)
                 (cond


                  ((eq? #t khz) `((khz . ,*freq*))) ;; <-- simple getter

                  ;; seek up/down
                  ((or (equal? khz "up") (equal? khz "down"))
                   (thread-start! (make-thread
                                   (->> (lambda ()
                                          (thread-sleep! 0.5)
                                          (set! *freq* (+ (if (equal? "up" khz) +1 -1) *freq*)))
                                        (loop/timeout 5)
                                        (loop))))
                   `((status . "success")))

                  ;; seek to specific frequency
                  ((string->number khz) =>
                   (lambda (khz)
                     (set! *freq* khz)
                     `((status . "success"))))

                  (else (error "khz must be (number? | up | down)" khz))))
               '(khz #t)))

;; ==================== fm (catalog) notifications ====================
(define *catalog-notify-connections* '())
(define-handler /v1/catalog/notify
  (make-notify-handler (getter-with-setter
                        (lambda () *catalog-notify-connections*)
                        (lambda (new) (set! *catalog-notify-connections* new)))))

;; do a `curl localhost:5060/v1/catalog/notify` and check with
;; (send-notification "foobar" `() 0 *catalog-notify-connections*)

(define (fm-thread-iteration)
  (send-notification "/v1/catalog/fm/seek"
                     `((khz . ,*freq*))
                     *server-port*
                     (getter-with-setter (lambda () *catalog-notify-connections*)
                                         (lambda (new) (set! *catalog-notify-connections* new)))))

(begin
  (handle-exceptions e (void) (thread-terminate! fm-thread))
  (define fm-thread
    (thread-start!
     (make-thread
      (->> (lambda () (fm-thread-iteration))
           (loop/interval 2)
           (loop/exceptions (lambda (e) (pp `(error: ,(current-thread)
                                                ,(condition->list e)))
                               #t))
           (loop))
      "fm-thread"))))
