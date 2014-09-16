
;;; Adds urls for browsing a pretend audio-source in our new nested
;;; json format.

(define-handler /v1/catalog/dab
  (lambda () `((preload . #( ((title . "Radio Stations") (uri . "/catalog/dab/stations"))))
          ;; TODO: remove
          (tabs . #( ((title . "Radio Stations") (uri . "/catalog/dab/stations")))))))


(define-handler /v1/catalog/dab/stations
  (lambda () (make-search-result 10 0 333
                            `( ((title . "NRK P1")         (turi . "file:///tmp/x.mp3"))
                               ((title . "NRK P2")         (turi . "file:///tmp/x.mp3"))
                               ((title . "NRK P3")         (turi . "file:///tmp/x.mp3"))
                               ((title . "NRK P4")         (turi . "file:///tmp/x.mp3"))
                               ((title . "NRK P5")         (turi . "file:///tmp/x.mp3"))
                               ((title . "NRK MP3")        (turi . "file:///tmp/x.mp3"))
                               ((title . "Radio Norge")    (turi . "file:///tmp/x.mp3"))
                               ((title . "Radio Grenland") (turi . "file:///tmp/x.mp3"))
                               ((title . "NRK Klassisk")   (turi . "file:///tmp/x.mp3"))
                               ((title . "The Voice")      (turi . "file:///tmp/x.mp3"))
                               ((title . "MTV")            (turi . "file:///tmp/x.mp3"))))))
