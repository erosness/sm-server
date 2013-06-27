(use test slip)

(test "end" "a"             (slip-read (open-input-string "a\300b")))
(test "eof" #f              (slip-read (open-input-string "")))
(test "esc end" "\334"      (slip-read (open-input-string "\333\334\300")))
(test "esc esc" "\335"      (slip-read (open-input-string "\333\335\300")))
(test-error "esc error"     (slip-read (open-input-string "\333\336\300")))

(test-error "no end, but eof" (slip-read (open-input-string "a")))

;; the empty packet "" could also be allowed here
;; the first \300 is an empty packet, but nobody wants it
(test "drop empty packet" "a" (slip-read (open-input-string "\300a\300")))

(test-group
 "subsequent packets"
 (define p (open-input-string "a\300b\300"))
 (test "packet 1 from stream" "a" (slip-read p))
 (test "packet 2 from stream" "b" (slip-read p))
 (test "no packets left" #f (slip-read p)))

