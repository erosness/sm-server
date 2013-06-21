(use test bitstring)

;; convenience macro for constructing blobs
;; (why is this not part of core, actually?)
;; note that there is a '#${ 00 ff } blob reader-macro for chicken,
;; but blobs cannot contain comments.
;; use like this: (blob 00 01 0a  10 1a  aa  ff FF)
(define-syntax blob-string
  (er-macro-transformer
   (lambda (x r t)
     (apply string
            (map (lambda (x) (integer->char (string->number (conc x) 16)))
                 (cdr x))))))

(define-syntax blob
  (syntax-rules ()
    ((_ hex ...) (string->blob (blob-string hex ...)))))


(test "bitstring default endian"
      "\x00\xff\x00\xff"
      (blob->string
       (bitstring->blob
        (bitconstruct (#xff00ff 32)))))

(test-group
 "node addresses"
 (test 'sl-station (describe-node #x02100100))
 (test #x0fFFffFF (describe-node #x0fFFffFF))

 (test #x02100100 (symbol->node-address 'sl-station))
 (test #f (symbol->node-address 'unknown-node!)))


(test
 "parse item-set-response"
 '(frame #x0362 (item-set-response ok))
 (parse-frame (blob 03 62 ;; fid
                    84    ;; item-set-response
                    01 00 ;; status: 00 => ok
                    )))

(test
 "parse item-get-response"
 '(frame #x1314 (item-get-response (ok . "ABC")))
 (parse-frame (blob 13 14    ;; fid
                    80       ;; type
                    01       ;; number of get responses
                    00       ;; status: ok
                    00 03    ;; blob-length
                    41 42 43 ;; blob body
                    )))

(test-group
 "parse notification"
 (test
  "udls notification"
  '(notification udls "ABC")
  (parse-notification "\x02\x11\x00\x00ABC"))

 (test
  "unknown node-address notification"
  '(notification #x01020304 "FOO")
  (parse-notification "\x01\x02\x03\x04FOO"))

 (test
  "udls notification with frame"
  '(frame #x1314 (notification udls "ABC"))
  (parse-frame (blob 13 14
                     c6
                     02 11 00 00
                     41 42 43))))

(test
 "parse unknown frame"
 `(frame #x0102 (unknown #x11 "ABC"))
 (parse-frame (blob 01 02 11 41 42 43)))

(test-error (parse-frame "\x00\x01"))

(test
 "parse list-get-response ok"
 `(frame 3 (list-get-response ok ("EPG Cambridge   \x00")))
 (parse-frame "\x00\x03\x81\x00\x00\x00\x00\x01\x01\x00\x11EPG Cambridge   \x002"))

(test
 "parse list-get-response fail"
 `(frame 3 (list-get-response fail "\231"))
 (parse-frame "\x00\x03\x81\205\231"))

(test-group
 "parse-status-codes"
 (test '(fail ok fail) (unpack-status-codes 3 "\x85\x00\x85"))
 (test '(ok)           (unpack-status-codes 1 "\x00"))
 (test '(1) (unpack-status-codes 1 "\x01"))
 (test-error (unpack-status-codes 2 "\x00"))
 (test-error (unpack-status-codes 0 (bitstring-of-any "\x00"))))



;;; ********** testing serialization

(test
 "set dab-state on"
 (blob 00 64       ;; id
       04          ;; itemset
       01          ;; item-count
       02 01 00 00 ;; node address (dab_state)
       00 01       ;; count
       01          ;; on/off
       ef          ;; checksum
       )
 (bitstring->blob (dab-set-state 100 #t)))

(test
 "set dab-state off"
 (blob ff ee       ;; id
       04          ;; itemset
       01          ;; item-count
       02 01 00 00 ;; node address (dab_state)
       00 01       ;; count
       00          ;; on/off
       ef          ;; checksum
       )
 (bitstring->blob (dab-set-state #xffee #f)))

(test
 "set get-list-item"
 (blob 00 68       ;; fid
       01          ;; cmd / type
       02 10 0d 00 ;; node-address
       00 00 00 17 ;; index
       02          ;; field-count
       04          ;; short-field
       05          ;; long-field
       00          ;; filter-count
       00          ;; sort-count
       ef)
 (bitstring->blob (dab-get-uservice #x0068 #x0017)))


(test
 "set dab-station"
 (blob 00 04       ;; fid
       04          ;; type: item-set
       01          ;; number of items
       02 10 01 00 ;; node-address
       00 04       ;; payload-length
       00 00 00 17 ;; payload #x17 => 23
       ef          ;; checksum
       )
 (bitstring->blob (dab-set-station 4 23)))
