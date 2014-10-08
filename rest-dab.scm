(use restlib dab matchable looper)
(use dab-i2c) ;; or (import dab-i2c turi)

(import turi)


;; turn on the actual radio module
(dab-turn-on)

;; turn off mute default
(dab-command (audio.attenuation 0))

(define-handler /v1/catalog/dab
  (lambda () `((preload . #( ((title . "Radio Stations") (uri . "/catalog/dab/stations")))))))

(define-turi-adapter channel->turi "dab"
  (lambda (params)
    (let* ((chidxstr (alist-ref 'id params))
           (chidx (string->number chidxstr)))
      (pp `(rest-dab station ,chidx))
      (match (dab-command (dab.sl.station chidx))
        ('(item-set-response FS_OK)
         ;; TODO: find IP so zones can reach DAB
         `((url . "http://127.0.0.1:8090/dab/hi")))
        (anything (error (conc "cannot set channel " chidxstr) anything))))))

(define-handler /v1/catalog/dab/stations
  (pagize
   (lambda ()
     (map (lambda (idx.name)
            (let* ((channel (cdr idx.name))
                   (index (car idx.name))
                   (turi (channel->turi `((id . ,index)))))
              `((title . ,channel)
                (turi . ,turi))))
          (dab-channels)))))

;; start querying for stations. dab-channels is filled from dab-read-thread.
(begin
  (handle-exceptions e (void) (thread-terminate! dab-channels-thread))
  ;; (thread-state dab-channels-thread)
  (define dab-channels-thread
    (thread-start!
     (->> (lambda ()
            (dab-refresh-channels!)
            #f) ;; <-- exit thread on successful completion
          (loop/exceptions (lambda (e) (pp `(error DAB channels ,(condition->list e) )) #t))
          (loop/interval 10)
          (loop)))))


;; helper utils. TODO: allow us to output pretty HTML here with a meta
;; refresh tag!
(define-handler /v1/catalog/dab/debug
  (lambda ()
    `((tuneStatus . ,(symbol->string (parse-dab.tuneStatus (dab-command (dab.tuneStatus)))))
      (dab.scan.state . , (symbol->string (parse-dab.scan.state (dab-command (dab.scan.state)))))
      (dab.sl.station . , (parse-dab.sl.station))
      (audio.sampleRate . ,(conc (dab-command (audio.sampleRate)))))))

(define-handler /v1/catalog/dab/buzzer
  (lambda ()
    (dab-reset)
    (dab-command (audio.buzzer.state 'on))
    (dab-command (audio.buzzer.frequency 440))))
