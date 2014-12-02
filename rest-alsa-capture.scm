(use restlib)

(import turi)

(define-turi-adapter bt->turi "bt"
  (lambda (params)
    `((url . "http://127.0.0.1:8090/bt"))))

(define-handler /v1/catalog/bt
  (lambda ()
    `((turi . ,(bt->turi '()))
      (title . "Bluetooth")
      (type . "bt")
      (image . "http://www.kirya.net/wp-content/uploads/2007/07/bluetooth_logo.png"))))
