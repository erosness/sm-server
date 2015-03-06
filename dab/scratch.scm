(use srfi-18 bitstring dab)

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




;;; FM example
;; Start dab station from app

(use dab matchable bitstring)
;; eval stuff from dab.scm
(fm-turn-on)

;; Not sure if this makes a difference
(dab-command (fm.forceToMono 'on))

;; You should get around -62 db
(fm-signal-strength)

;; 0 is 'all, 1 is 'strong
(dab-command (fm.searchLevel))

;; Usully have to set it to all to find anything while searching
(dab-command (fm.searchLevel 'strong))

;; Should get you something eventually
(dab-command (fm.search 'up))
(dab-command (fm.search))

;; current frequency
(fm-frequency)
(fm-frequency 94850)
;; Station name or #f
(fm-radio-text)

;; Known to work
(fm-frequency 99000) ;; NRK P3
(fm-frequency 94800) ;; NRK P2 KULTURKANALEN



