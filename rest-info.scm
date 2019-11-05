(include "debug-utils.scm")

(module* rest-info ()

(import chicken scheme data-structures srfi-1)

(use intarweb spiffy
     medea matchable irregex ports clojurian-syntax restlib srfi-18 extras
     posix srfi-1 srfi-13
     (only posix with-input-from-pipe))

;; local imports
(import restlib store)

;; Use ethernet MAC as unique id
(cond-expand
  (arm
    (define (uid) (with-input-from-file "/sys/class/net/eth0/address" read-string)))
  (else
    (define (uid) (with-input-from-file "/sys/class/net/enp0s31f6/address" read-string))))

(define (capability uid)
  (match uid
    ("4c:cc:6a:d7:c1:78\n" '#("doorlock" "this-is-host"))
    ("b8:27:eb:33:47:88\n" '#("doorlock" "doorbell-out" "door-motor"))
    ("b8:27:eb:4c:26:ae\n" '#("presence" "doorbell-in" "shade-motor"))
    ("b8:27:eb:6f:22:ad\n" '#("presence" "floor"))
    ("b8:27:eb:c6:f9:04\n" '#("presence" "floor" "shade-motor"))
    (_ #("no-service"))))

(define (unit-name uid)
  (match uid
    ("4c:cc:6a:d7:c1:78\n" "host")
    ("b8:27:eb:33:47:88\n" "pi2")
    ("b8:27:eb:4c:26:ae\n" "pi3a")
    ("b8:27:eb:6f:22:ad\n" "pi3b")
    ("b8:27:eb:c6:f9:04\n" "pi3c")
    (_ #("no-name"))))

(define (empty-value)
  `((name . ,(unit-name(uid)))
    (cap . ,(capability (uid)))
    (uid  . ,(uid))))

(define-handler /v1/sm/info
  (lambda ()
    (empty-value)))
)
