(use test slip srfi-1)

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

(test-group
 "slip-write"
 (define (sw str) (with-output-to-string (lambda () (slip-write str))))

 (test "wrap in END"          "\300A\300"          (sw "A"))
 (test "escape end"           "\300\333\334\300"   (sw "\300"))
 (test "escane esc"           "\300\333\335\300"   (sw "\333"))

 (test "bigger string " "\300ABCD\300" (sw "ABCD"))

 (test-begin "byterange unchange (except END and ESC)")
 (for-each (lambda (char)
             (test (conc "slip-write char "(char->integer char)) ;; <- description
                   (conc "\300" char "\300") ;; <-- expected
                   (sw (conc char)))) ;; <-- actual
           (remove (lambda (x) (or (eq? (integer->char #o300) x)
                              (eq? (integer->char #o333) x)))
                   (map integer->char (iota 256))))
 (test-end))

