(cond-expand
 ((and arm) (use dab))
 (else))

(use test restlib clojurian-syntax looper fmt bitstring matchable)
(import rest notify turi incubator)

(import rest-player)
;; ==================== fm (catalog) notifications ====================
(define *catalog-notify-connections* '())

(define-handler /v1/catalog/notify
  (make-notify-handler (getter-with-setter
                        (lambda () *catalog-notify-connections*)
                        (lambda (new) (set! *catalog-notify-connections* new)))))

;; do a `curl localhost:5060/v1/catalog/notify` and check with
;; (send-notification "foobar" '((yo . "yo")) (getter-with-setter
;;                                  (lambda () *catalog-notify-connections*)
;;                                  (lambda (new) (set! *catalog-notify-connections* new))))


(define dab-version "V5.1.7.52015-2")

(define (ensure-fm-on)
  (dab-abort-if-scanning)
  (if (not (fm-on?)) (fm-turn-on)))


(define (turi-command params)
  (or (and-let* ((hz (alist-ref 'hz params))
                 (val (string->number hz)))
        (ensure-fm-on)
        (fm-frequency val)
        ;; TODO: find IP so zones can reach DAB
        `((url . "default:CARD=imxaudiovenice9")))
      (error "invalid fm params. expected hz key with number value")))

(define-local-turi-adapter fmfreq->turi "fm"
  turi-command)

;; (hz-pretty-print 102550) => "102.55Mhz"
(define (hz-pretty-print hz) (fmt #f (num (/ hz 1000) 10 2) "Mhz"))


;; TODO: make all fm/dab apis not throw?
(define (fm-radio-text-safe)
  (handle-exceptions e
    (begin
      (pp (condition->list e))
      #f)
    (fm-radio-text)))

(define (fm-get-state)
  (let* ((freq (fm-frequency))
         (turi-alist `((hz . ,freq))))
    `((title . ,(fm-radio-ps))     
      (type . "fm")
      (frequency . ,freq)
      (tuneStatus . ,(symbol->string (fm-tunestatus)))
      (subtitle . ,(fm-radio-text-safe))
      (turi . ,(fmfreq->turi turi-alist))
      (signalStrength . ,(fm-signal-strength)))))

(define (fm-searching?)
  (eq? (fm-tunestatus) 'idle))

;;; -------- Search
(define fm-search-iteration
  (lambda ()
    ;; Check if we're still searching before sending the
    ;; notifications, this way the last msg we send out will be the
    ;; state _after_ the search stopped
    (let ((searching? (fm-searching?))
          (state (fm-get-state)))
      (send-notification "/v1/catalog/fm/seek"
                         ;; TODO: turi host always resolves to localhost
                         ;; here, since we don't have a current request
                         ;; to look at. Remove it to avoid confusion.
                         (alist-delete 'turi state)
                         (getter-with-setter (lambda () *catalog-notify-connections*)
                                             (lambda (new) (set! *catalog-notify-connections* new))))
      (fm-pq)
      ;; Keep looping?
      searching?)))

(define (notify-fm-search-state interval)
  (define notify-thread (make-thread
                         (->> fm-search-iteration
                              (loop/interval interval)
                              (loop/exceptions (lambda (e) (pp `(error: ,(current-thread)
                                                                   ,(condition->list e)))
                                                  ;; halt on exceptions
                                                  #f))
                              (loop))
                         "fm-notify-thread"))
  (thread-start! notify-thread))

;; Thread safety
(define *fm-notify-thread* (make-atom #f))

(define (fm-notify-alive?)
  (and (thread? (*fm-notify-thread*))
       (not (eq? (thread-state (*fm-notify-thread*))
                 'dead))))

;; FM scan

(define fm-scan-iteration
  (lambda ()
    (fm-pq)
    ;; Keep looping?
    (fm-step-scan fm-current-step)
    (let ((fq (fm-scan-end?)))
      (if fq
	(begin
	  (fm-frequency (car fq))
	  #f)
        #t))))


(define (fm-scan-worker step)
  (define fm-scan-thread (make-thread
                         (->> fm-scan-iteration
                              (loop/interval 1) ;; 1s to let power indicator stab.
                              (loop/exceptions (lambda (e) (pp `(error: ,(current-thread)
                                                                   ,(condition->list e)))
                                                  ;; halt on exceptions
                                                  #f))
                              (loop))
                         "fm-scan-thread"))
  (thread-start! fm-scan-thread))

(define *fm-scan-thread* (make-atom #f))


(define (next-target-frequency current-frequency step)
  (let ((fm-first-frequency 87500)
	(fm-last-frequency 108000))
    (cond
     ((and (> step 0) (= current-frequency fm-last-frequency))
	  (fm-frequency fm-first-frequency)
	  (fm-start-scan step))
     ((and (< step 0) (= current-frequency fm-first-frequency)
	  (fm-frequency fm-last-frequency)
	  (fm-start-scan step)))
     (else
          (+ current-frequency step)))))

(define fm-search-vector '())
(define fm-scan-step-up    100)
(define fm-scan-step-down -100)
(define fm-current-step #f)
(define fm-current-initial-frequency #f)

;; Detecting a station: The FM signal energy from av station occupies
;; about 3 frequency bins. Thus a station is detected by 5 samples, where
;; average level of the 3 mid samples are stronger than the average of the
;; two side samples. In addition the signal strength must be better than
;; -50
;;Returns frequency/strength-pair, or #f if no station.
;;
;; Note that this code works on the collected list only, and does not
;; even access the module. During intended use the module is tuned two steps 
;; off the frequency when the procedure is run, and must be set by the calling
;; code.

(define (fm-detect-signal?)
  (let ((fm-detection-threshold -7)
	(fm-signal-strength-threshold -65)
	(s1 (cdr (car (cddddr fm-search-vector))))
	(s2 (cdr (car (cdddr fm-search-vector))))
        (s3 (cdr (car (cddr fm-search-vector))))
        (s4 (cdr (car (cdr fm-search-vector))))
	(s5 (cdr (car fm-search-vector))))
    (let* ((avg-side (/ (+ s1 s5) 2))
	   (avg-mid (/ (+ s2 s3 s4) 3))
	   (delta (- avg-side avg-mid)))
      (print "D=" delta " S=" s3 " f=" (car (car ( cddr fm-search-vector))))
      (if (and (< delta fm-detection-threshold)
	 (> s3 fm-signal-strength-threshold))
	  (car ( cddr fm-search-vector))
	  #f))))

(define (fm-scan-reached-initial? frequency)
  (cond
   ((not fm-current-initial-frequency) #t)
   ((<= (abs (- frequency fm-current-initial-frequency)) 100) #t)
   (else #f)))

;; To get rid of noise each reading of signal strength is done
;; three times and the average is presented.
(define (fm-avg-signal-strength)
  (/ (+ (fm-signal-strength) (fm-signal-strength) (fm-signal-strength)) 3))

(define (fm-step-scan step)
  (let* ((current-strength (fm-avg-signal-strength))
	 (current-frequency (fm-frequency))
         (target-frequency (next-target-frequency current-frequency step)))
    (set! fm-search-vector (cons (cons current-frequency current-strength) fm-search-vector))
    (fm-frequency target-frequency)))
    
(define (fm-start-scan step)
  (let* ((start-frequency (+ (fm-frequency) step))
 	 (start-vector (cons start-frequency (fm-avg-signal-strength)))
	 (target-frequency (next-target-frequency start-frequency step))
	 (noise (fm-avg-signal-strength)))
    (set! fm-search-vector
      (list start-vector '(0 . -70) '(0 . -70) '(0 . -70) '(0 . -70)))
    (fm-frequency target-frequency)
    (sleep 1)))

(define (fm-scan-end?)
  (if (fm-scan-reached-initial? (fm-frequency))
    (cons (fm-frequency) (fm-avg-signal-strength))
    (fm-detect-signal?)))
	   
;; Start searching
(define (fm-scan step)
  (if (= step 0)
    (set!  fm-current-initial-frequency #f)
    ((set! fm-current-initial-frequency (fm-frequency))
     (set! fm-current-step step)
     (fm-start-scan step)
     (*fm-scan-thread* (fm-scan-worker 1)))))


(define (fm-search-with-notify direction)
  (fm-search direction)
  (if (not (fm-notify-alive?))
      (*fm-notify-thread* (notify-fm-search-state 1))))

;; REST interface

(define-handler /v1/catalog/fm/seek
  (argumentize (lambda (hz)
                 (ensure-fm-on)
                 (cond
                  ((eq? hz #t)
                   (fm-get-state))

                  ((equal? "up" hz)
		   (if (string-contains (radio-version) dab-version)
		     
		       (fm-search-with-notify 'up)
		       (fm-scan fm-scan-step-up)
		       )
                   `((status . "ok")))

                  ((equal? "down" hz)
		   (if (string-contains (radio-version) dab-version)
		       (fm-search-with-notify 'up)
		       (fm-scan fm-scan-step-down)
		       )
                   `((status . "ok")))

                  ((string->number hz) =>
                   (lambda (hz)
                     ;; stop any ongoing searches, this also has the
                     ;; nice side-effect of notifying clients that the
                     ;; frequency has changed
		     (if (string-contains (radio-version) dab-version)
			 (fm-search-with-notify 'idle)
			 (fm-scan 0)
			 )
                     (fm-frequency hz)

		     (fm-pq)
                     `((status . "ok"))))))
               '(hz #t)))


