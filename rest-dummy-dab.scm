
;; Mock failure
;; After a request to /v1/catalog/dab/fail dab/fm routes will return
;; 503 Service Unavailable for 30 seconds. This emulates the initial
;; dab scan as needed

(define dab-scanning? #f)
(define fail-timer
  (lambda _
    (set! dab-scanning? #t)
    (thread-sleep! 30)
    (set! dab-scanning? #f)))

;; Note: this needs to match the definition in rest-dab.scm
;; TODO: don't duplicate
(define (dab-abort-if-scanning)
  (if dab-scanning?
      (abort (make-property-condition 'dab-scanning))))

(define-handler /v1/catalog/dab/fail
  (lambda ()
    (if (not dab-scanning?)
        (begin
          (thread-start! fail-timer)
          "failing for the next 30 seconds")
        "already failing")))

;;; Adds urls for browsing a pretend audio-source in our new nested
;;; json format.

(define-handler /v1/catalog/dab
  (lambda () `((preload . #( ((title . "Radio Stations") (uri . "/catalog/dab/stations")))))))

(define-handler /v1/catalog/dab/stations
  (lambda ()
    (dab-abort-if-scanning)
    (make-search-result 10 0 333
                        `( ((title . "NRJ NORGE")        (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK P13")          (turi . "file:///tmp/x.mp3"))
                           ((title . "P6 Rock")          (turi . "file:///tmp/x.mp3"))
                           ((title . "P5 Hits")          (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK KLASSISK")     (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK SPORT")        (turi . "file:///tmp/x.mp3"))
                           ((title . "P4 TPEG")          (turi . "file:///tmp/x.mp3"))
                           ((title . "P4 LydenAvNorge")  (turi . "file:///tmp/x.mp3"))
                           ((title . "radio 1")          (turi . "file:///tmp/x.mp3"))
                           ((title . "-RADIO NORGE-")    (turi . "file:///tmp/x.mp3"))
                           ((title . "P7 Klem")          (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK JAZZ")         (turi . "file:///tmp/x.mp3"))
                           ((title . "#RadioRock")       (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK EPG")          (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK P2")           (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK P1")           (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK P1 Telemark")  (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK P1+Buskerud")  (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK P1+Vestfold")  (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK SUPER")        (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK P3")           (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK P1+")          (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK mP3")          (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK Vær SØR")      (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK NYHETER")      (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK SÁMI RADIO")   (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK P1+Telemark")  (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK P1 Buskerud")  (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK P1 Vestfold")  (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK P1 Sogn & Fj") (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK Vær VEST")     (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK P1 Hordaland") (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK P1")           (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK NYHETER")      (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK EPG")          (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK mP3")          (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK P2")           (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK P3")           (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK P1+ Hord")     (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK SÁMI RADIO")   (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK P1+ So&Fj")    (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK P1+")          (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK SUPER")        (turi . "file:///tmp/x.mp3"))
                           ((title . "NRK SÁMI EXTRA")   (turi . "file:///tmp/x.mp3"))
                           ))))

