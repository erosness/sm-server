(use srfi-18 bitstring dab dab-i2c)

;; (dab-command (fm.state #f))
(dab-command (dab.state #t))

(dab-command (dab.scan.state #t))
(dab-command (dab.udls))
(dab-command (dab.tuneStatus))
(dab-command (dab.sl.station 7))
(dab-command (misc.clock.localTime))
(dab-command (audio.sampleRate))


(dab-command (fm.state #t))

(dab-command (fm.frequency))
(dab-command (fm.frequency #:notify #t))
(dab-command (fm.frequency 87500))
(dab-command (fm.frequency 97500))

(dab-command (fm.tuneStatus))

(dab-command (fm.frequency 100000))
(dab-command (fm.search))
(dab-command (fm.search 'down))
(dab-command (fm.search 'up))

(dab-command (fm.signalStrength))
(dab-command (fm.rds.active))

(dab-command (fm.rds.ps))
(dab-command (fm.rds.pty))
(dab-command (fm.rds.radioText))
(dab-command (fm.rds.radioText #:notify #t))

(dab-command (fm.searchLevel))

