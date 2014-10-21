(use test restlib clojurian-syntax)
(import rest notify)

(define (fm-send-audio! khz #!optional (duration 0.5))
  ;; send a sound snippet to cplay
  (with-output-to-pipe
   (conc "ffmpeg -loglevel quiet -f lavfi "
         " -i \"aevalsrc=sin(" khz "*2*PI*t):d=" duration "\" "
         " -f s8 udp://localhost:3000?listen")
   void))

;; (fm-send-audio! 1000)
;; (fm-send-audio! 440 7)

(define *freq* 44000)

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
  (lambda (params)
    (let ((khz (alist-ref 'id params)))
     `((url . "udp://localhost:3000")
       (format . "s8") ;; <-- see "-f s8"
       ;; if live is #t and the turi is the same as what cplay is
       ;; currently , we don't need to re-initialize cplay because it'd
       ;; be playing the very same sound. if live is #f, cplay still
       ;; needs to be re-initialized because we might want to play from
       ;; the start.
       (live . #t)))))

(define-handler /v1/catalog/fm/browse
  (lambda () `((turi . ,(fmfreq->turi '((id . "ignored")))))))

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
