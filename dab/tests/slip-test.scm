(use test)

(test "end" "a"             (read-slip (open-input-string "a\300b")))
(test "eof" #f              (read-slip (open-input-string "")))
(test "esc end" "\334"      (read-slip (open-input-string "\333\334\300")))
(test "esc esc" "\335"      (read-slip (open-input-string "\333\335\300")))
(test-error "esc error"     (read-slip (open-input-string "\333\336\300")))

(test-error "no end, but eof" (read-slip (open-input-string "a")))

;; the empty packet "" could also be allowed here
;; the first \300 is an empty packet, but nobody wants it
(test "drop empty packet" "a" (read-slip (open-input-string "\300a\300")))

(test-group
 "subsequent packets"
 (define p (open-input-string "a\300b\300"))
 (test "packet 1 from stream" "a" (read-slip p))
 (test "packet 2 from stream" "b" (read-slip p))
 (test "no packets left" #f (read-slip p)))

