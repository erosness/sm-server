;; Returns access procedures to GPIO, alternatively an emulated version.

(module gpio *

(import chicken scheme extras)

(define (invert lvl)
  (lambda ()(if (= (lvl) 0) 1 0)))

(cond-expand
  (arm
    (use raspberry-pi-gpio)
    (setup-physical)
    (define (make-gpio-input p)
      (pin-mode p 'input')
      (pull-up-dn-control p 'up')
      (lambda () (digital-read p )))

    (define (make-gpio-output p)
      (pin-mode 12 'output')
      (lambda (val) (digital-write p val)))

    (print "Gpio arm"))
(else
  (define (filename n)
    (string-append "/var/sm/p" (number->string n)))

  (define (make-gpio-input p)
    (gpio-output p #f)
    (lambda ()(with-input-from-file (filename p)
      (lambda()(if (eqv? #\0 (string-ref (read-string) 0)) 0 1 )))))

  (define (make-gpio-output p)
    (gpio-output p #f)
    (lambda (val)
      (gpio-output p val)))

  (define (gpio-output p val)
    (if (file-exists? (filename p))
      (delete-file (filename p)))
    (with-output-to-file (filename p) (lambda () (write-string (if (or (eqv? val 0) (not val)) "0" "1")))))
  ))
)
