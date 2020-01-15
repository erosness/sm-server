;; Configuration for sm-demp. Set certain objects based on Ethernet
;; MAC address of target.
(module sm-config ( uid
                    capability
                    unit-name )

(import chicken scheme data-structures srfi-1 extras matchable utils irregex posix)

;; Get the unit id (uid), which is the string representation of the
;; MAC addrss of the first (often only) ethernet interface found.
(define (is-eth? f)
  (irregex-search "^e[nt]" f))

(define (eth-path)
  (conc "/sys/class/net/"(find is-eth? (directory "/sys/class/net")) "/address"))

(define (uid)
  (substring
    (with-input-from-file (eth-path) read-string) 0 17))


(define (capability)
  (match (uid)
    ("4c:cc:6a:d7:c1:78" '("doorbell-in" "floor"))
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
