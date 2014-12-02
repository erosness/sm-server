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

(define-handler /v1/catalog/bt
  (lambda ()
    `((turi . ,(bt->turi '((d . "default:CARD=imxaudiobtm720"))))
      (title . "Bluetooth")
      (type . "bt")
      (image . "http://www.kirya.net/wp-content/uploads/2007/07/bluetooth_logo.png"))))
