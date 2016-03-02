;;; $ adb shell alsa_arecord -L | grep CARD
;;; default:CARD=imxspdif
;;; default:CARD=imxaudiovenice9
;;; default:CARD=imxaudiobtm720
;;; default:CARD=imxaudiotr1
;;;
;;; OBS: venice9 is "slightly" more complex and used in rest-dab.scm
(use restlib)

(import turi)

(define-turi-adapter bt->turi "alsa"
  (lambda (params)
    `((url . ,(alist-ref 'd params))
      (format . "alsa"))))

(define-handler /v1/catalog/line-in
  (lambda ()
    `((turi . ,(bt->turi '((d . "default:CARD=imxaudiotr2"))))
      (title . "Line-In")
      (type . "line-in")
      (image . "http://www.futureshop.ca/multimedia/Products/500x500/102/10200/10200642.jpg"))))

(define-handler /v1/catalog/toslink
  (lambda ()
    `((turi . ,(bt->turi '((d . "default:CARD=imxspdif"))))
      (title . "Toslink")
      (type . "toslink")
      (image . "http://www.thonet-vander.com/img/icon/spdif.png"))))
