(use test clojurian-syntax)


;; these values are taken from Sigma Studio's TableGen (Tools->FilterTableGen)
;; coefficient lists always contain b0 b1 b2 a1 a2 in that order

;; allow some slack because Sigma's tablegen produce 32bit output,
;; where we use f64 all the way (we don't need to be fast)
(current-test-epsilon 1e-3)


(define (test-element a b) (test (conc a " == " b) a b))


(for-each
 test-element
 (list 1.08201718330383 ;;,        0x00 ,        0x8A ,        0x7F ,        0x8A ,
       -1.96271848678589 ;;,        0xFF ,        0x04 ,        0xC5 ,        0xA4 ,
       0.882503747940063 ;;,        0x00 ,        0x70 ,        0xF5 ,        0xE2 ,
       1.96271848678589 ;;,        0x00 ,        0xFB ,        0x3A ,        0x5C ,
       -0.964520931243896) ;;,        0xFF ,        0x84 ,        0x8A ,        0x94 ,

 (-> (biquad-coefficients 15 150 22000 0.5)
     (invert-a-coefficients)
     (f64vector->list)))


(for-each
 test-element
 (list 0.962069749832153 ;;,        0x00 ,        0x7B ,        0x25 ,        0x1A ,
       -1.8080747127533 ;;,        0xFF ,        0x18 ,        0x91 ,        0x02 ,
       0.864597797393799 ;;,        0x00 ,        0x6E ,        0xAB ,        0x24 ,
       1.8080747127533 ;;,        0x00 ,        0xE7 ,        0x6E ,        0xFE ,
       -0.826667666435242) ;;,        0xFF ,        0x96 ,        0x2F ,        0xC1 ,


 (-> (biquad-coefficients -5 500 22000 1)
     (invert-a-coefficients)
     (f64vector->list)))

(test-begin "tablegen-test for 100hz, 44100sps Q=0.71")

(define (1000hz-table gain)
  (-> (biquad-coefficients  gain 1000 44100 0.71)
      (invert-a-coefficients)
      (f64vector->list)))

(define (gain-test expected actual)
  (map test-element expected actual))

(gain-test (list 0.842409253120422
                 -1.60026705265045
                 0.774238467216492
                 1.60026705265045
                 -0.616647720336914
                 )
           (1000hz-table -15))
(gain-test (list 0.879036903381348
                 -1.65567564964294
                 0.793586492538452
                 1.65567564964294
                 -0.672623515129089
                 )
           (1000hz-table -11.6666666666667))
(gain-test (list 0.914201736450195
                 -1.70438575744629
                 0.80763053894043
                 1.70438575744629
                 -0.721832275390625
                 )
           (1000hz-table -8.33333333333333))
(gain-test (list 0.948506116867065
                 -1.74680423736572
                 0.816178798675537
                 1.74680423736572
                 -0.764684915542603
                 )
           (1000hz-table -5))
(gain-test (list 0.982688546180725
                 -1.78344058990479
                 0.819007754325867
                 1.78344058990479
                 -0.801696300506592
                 )
           (1000hz-table -1.66666666666667))
(gain-test (list 1.01761651039124
                 -1.81485843658447
                 0.815819382667542
                 1.81485843658447
                 -0.833435773849487
                 )
           (1000hz-table 1.66666666666667))
(gain-test (list 1.05428946018219
                 -1.841637134552
                 0.806199193000793
                 1.841637134552
                 -0.860488653182983
                 )
           (1000hz-table 5))
(gain-test (list 1.09385049343109
                 -1.86434304714203
                 0.789576530456543
                 1.86434304714203
                 -0.883427023887634
                 )
           (1000hz-table 8.33333333333334))
(gain-test (list 1.1376086473465
                 -1.88351082801819
                 0.765182256698608
                 1.88351082801819
                 -0.902790904045105
                 )
           (1000hz-table 11.6666666666667))
(gain-test (list 1.18707144260406
                 -1.89963138103485
                 0.732005000114441
                 1.89963138103485
                 -0.919076442718506)
           (1000hz-table 15))

(test-end)
