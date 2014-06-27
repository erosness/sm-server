(use test restlib)
(import rest)

;; generate the static url for the FM audio payload stream
(define (fm-suri)
  `((url . ,(uri->string (update-uri (current-host)
                                     port: 1101
                                     path: '(/ "fm"))))
    ;; if live is #t and the turi is the same as what cplay is
    ;; currently , we don't need to re-initialize cplay because it'd
    ;; be playing the very same sound. if live is #f, cplay still
    ;; needs to be re-initialized because we might want to play from
    ;; the start.
    (live . #t)))

;; ==================== radio station list ====================

(define-turi-adapter fmrs->turi "fmrs"
  (lambda (station)
    ;; TODO station->frequency, then seek on hw
    (fm-suri)))

(define-handler /v1/catalog/fm
  (lambda () `((tabs . #( ((title . "FM Stations") (uri . "/catalog/fm/stations")))))))


(define-handler /v1/catalog/fm/stations
  (lambda () (make-search-result 10 0 333
                            `( ((title . "NRK P1")         (turi . ,(fmrs->turi "NRK P1")))
                               ((title . "NRK P2")         (turi . ,(fmrs->turi "NRK P2")))
                               ((title . "Radio Norge")    (turi . ,(fmrs->turi "Radio Norge")))))))


;; ==================== explicit frequency ====================

(define-turi-adapter fmfreq->turi "fmfreq"
  (lambda (khz)
    ;; TODO: tune radio to khz here
    (fm-suri)))

(define-handler /v1/catalog/fm/seek
  (argumentize (lambda (khz)
                 (cond

                  ((or (equal? khz "up") (equal? khz "down"))
                   `((turi . ,(fmfreq->turi khz))))

                  ;; construct explicit frequency turis
                  ((string->number khz) =>
                   (lambda (khz) `((turi . ,(fmfreq->turi khz)))))

                  (else (error "khz must be (number? | up | down)" khz))))
               'khz))
