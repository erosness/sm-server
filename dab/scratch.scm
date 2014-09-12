(use srfi-18 bitstring dab dab-i2c)

;; (dab-send (fm.state #f))
(dab-send (dab.state #t))

(dab-send (dab.scan.state #t))
(dab-send (dab.udls))
(dab-send (dab.tuneStatus))
(dab-send (dab.sl.station 7))
(dab-send (misc.clock.localTime))
(dab-send (audio.sampleRate))


(dab-send (fm.state #t))

(dab-send (fm.frequency))
(dab-send (fm.frequency #:notify #t))
(dab-send (fm.frequency 87500))
(dab-send (fm.frequency 97500))

(dab-send (fm.tuneStatus))

(dab-send (fm.frequency 100000))
(dab-send (fm.search))
(dab-send (fm.search 'down))
(dab-send (fm.search 'up))

(dab-send (fm.signalStrength))
(dab-send (fm.rds.active))

(dab-send (fm.rds.ps))
(dab-send (fm.rds.pty))
(dab-send (fm.rds.radioText))
(dab-send (fm.rds.radioText #:notify #t))

(dab-send (fm.searchLevel))
