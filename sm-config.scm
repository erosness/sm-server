;; Configuration for sm-demp. Set certain objects based on Ethernet
;; MAC address of target.
(module sm-config ( uid
                    capability
                    unit-name )

(import chicken scheme data-structures srfi-1 extras matchable)

;; Use ethernet MAC as unique id
(cond-expand
  (arm
    (define (uid)
      (substring
        (with-input-from-file "/sys/class/net/eth0/address" read-string) 0 17)))
  (else
    (define (uid)
      (substring
        (with-input-from-file "/sys/class/net/enp0s31f6/address" read-string) 0 17))))

(define (capability)
  (match (uid)
    ("4c:cc:6a:d7:c1:78" '("doorlock" "this-is-host" "doorbell-out"))
    ("b8:27:eb:33:47:88" '("doorlock" "doorbell-out" "door-motor"))
    ("b8:27:eb:4c:26:ae" '("presence" "doorbell-in" "shade-motor"))
    ("b8:27:eb:6f:22:ad" '("presence" "floor"  "doorbell-out"))
    ("b8:27:eb:c6:f9:04" '("presence" "floor" "shade-motor"))
    (_ ("no-service"))))

(define (unit-name)
  (match (uid)
    ("4c:cc:6a:d7:c1:78" "host")
    ("b8:27:eb:33:47:88" "pi2")
    ("b8:27:eb:4c:26:ae" "pi3a")
    ("b8:27:eb:6f:22:ad" "pi3b")
    ("b8:27:eb:c6:f9:04" "pi3c")
    (_ #("no-name"))))

)
