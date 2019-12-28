(import chicken scheme led-matrix)


(define (led-image-bell1)
  (begin
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(black)(red)(red)(black)(black)(black)
    (black)(black)(black)(red)(red)(black)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
  )
)

(define (led-image-bell2)
  (begin
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(red)(black)(black)(red)(black)(black)
    (black)(black)(black)(red)(red)(black)(black)(black)
    (black)(black)(black)(red)(red)(black)(black)(black)
    (black)(black)(red)(black)(black)(red)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
  )
)

(define (led-image-bell3)
  (begin
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(red)(black)(black)(black)(black)(red)(black)
    (black)(black)(red)(black)(black)(red)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(red)(black)(black)(red)(black)(black)
    (black)(red)(black)(black)(black)(black)(red)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
  )
)

(define (led-image-bell4)
  (begin
    (black)(red)(black)(black)(black)(black)(red)(black)
    (red)(red)(black)(black)(black)(black)(red)(red)
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
    (red)(red)(black)(black)(black)(black)(red)(red)
    (black)(red)(black)(black)(black)(black)(red)(black)
  )
)

(define (led-image-bell5)
  (begin
    (black)(red)(black)(black)(black)(black)(red)(black)
    (red)(black)(black)(black)(black)(black)(black)(red)
    (black)(black)(black)(black)(black)(black)(black)(black)
    (black)(black)(black)(red)(red)(black)(black)(black)
    (black)(black)(black)(red)(red)(black)(black)(black)
    (black)(black)(black)(black)(black)(black)(black)(black)
    (red)(black)(black)(black)(black)(black)(black)(red)
    (black)(red)(black)(black)(black)(black)(red)(black)
  )
)

(define led-image-bell
  (list
    led-image-bell1
    led-image-bell2
    led-image-bell3
    led-image-bell4
    led-image-bell5))
