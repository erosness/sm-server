(use blobbery test dsp)

;; I couldn't find a way to extract i2c on-the-wire data for safeload
;; writes. therefore the i2c safeload test-data gathered from our
;; previous java-implementation (EqBand.java), where coefficients were
;; copy-pasted into java from sigma studio's FilterTableGen. we should
;; gather more data here but it's tedious.

(test
 "raw safeload data"
 (list (blob-string 08 10 00 01 02 03 04)
       (blob-string 08 15 01 29)
       (blob-string 08 11 00 10 20 30 40)
       (blob-string 08 16 01 2A)
       (blob-string 08 1C 00 3C))

 (safeload `((#x0129 . ,(blob-string 01 02 03 04))
             (#x012A . ,(blob-string 10 20 30 40)))))


;; fs 48000, gain 5, f0 1000hz
(test
 "eq setting with safeload"
 (list
  (blob-string 08 10 00 00 86 6C 8B )
  (blob-string 08 15 01 24 )
  (blob-string 08 11 00 FF 12 8E 9E )
  (blob-string 08 16 01 25 )
  (blob-string 08 12 00 00 69 11 5B )
  (blob-string 08 17 01 26 )
  (blob-string 08 13 00 00 ED 71 62 )
  (blob-string 08 18 01 27 )
  (blob-string 08 14 00 FF 90 82 1A )
  (blob-string 08 19 01 28 )
  (blob-string 08 1C 00 3C ))
 (eq-packets 1 5 1000))
