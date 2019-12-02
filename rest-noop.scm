(module rest-dummy ()

(import chicken scheme)

(use medea srfi-69 data-structures)
;; local imports
(import restlib sm-config)

(define (fid uid cap)
  (string-hash (conc uid cap)))

(define-handler /v1/sm/doorlock/status
  (lambda ()
    `((fid . ,(fid (uid) "doorlock"))
      (a . 1)
      (b . "b")
      (c . 3))))

(define-handler /v1/sm/door-motor/status
  (lambda ()
    `((fid . ,(fid (uid) "door-motor"))
      (a . 1)
      (b . "b")
      (c . 3))))

(define-handler /v1/sm/presence/status
  (lambda ()
    `((fid . ,(fid (uid) "presence"))
      (a . 1)
      (b . "b")
      (c . 3))))

(define-handler /v1/sm/floor/status
  (lambda ()
    `((fid . ,(fid (uid) "floor"))
      (a . 1)
      (b . "b")
      (c . 3))))


)

;;    ("4c:cc:6a:d7:c1:78" '("doorlock" "doorbell-out"))
;;    ("b8:27:eb:33:47:88" '("doorlock" "doorbell-out" "door-motor"))
;;    ("b8:27:eb:4c:26:ae" '("presence" "doorbell-in" "shade-motor"))
;;    ("b8:27:eb:6f:22:ad" '("presence" "floor"  "doorbell-out"))
;;    ("b8:27:eb:c6:f9:04" '("presence" "floor" "shade-motor"))
