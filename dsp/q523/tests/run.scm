(use q523 test srfi-4)

(define (convert num)
  (u8vector->list (fp->q523 num)))

;; taken from i2c output from Sigma Studio

(test (list #x00  #x80  #x74  #x53) (convert 1.00354993343353))
(test (list #xFF  #x00  #x6D  #xD2) (convert -1.99664855003357))
(test (list #x00  #x7F  #x20  #x15) (convert 0.993166565895081))
(test (list #x00  #xFF  #x92  #x2E) (convert 1.99664855003357))
(test (list #xFF  #x80  #x6B  #x98) (convert -0.996716499328613))

(test-exit) 
