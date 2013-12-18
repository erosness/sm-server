(use test bitstring blobbery dab)

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
 '(frame #x0362 (item-set-response FS_OK))
 (parse-frame (blob 03 62 ;; fid
                    84    ;; item-set-response
                    01 00 ;; status: 00 => ok
                    ))) ;; magic checksum

(test
 "parse faulty item-set-response"
 `(frame #x1234 (item-set-response (error unexpected-eof)))
 (parse-frame (blob 12 34 ;;fid
                    84    ;; item-set-response
                    02    ;; number of responses
                    )))

(test
 "parse item-get-response"
 '(frame #x1314 (item-get-response (FS_OK . "ABC")))
 (parse-frame (blob 13 14    ;; fid
                    80       ;; type
                    01       ;; number of get responses
                    00       ;; status: ok
                    00 03    ;; blob-length
                    41 42 43 ;; blob body
                    )))      ;; magic checksum

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
                     41 42 43
                     ))))

(test
 "parse unknown frame"
 `(frame #x0102 (unknown #x11 "ABC"))
 (parse-frame (blob 01 02 11 41 42 43)))

(test '(error invalid-frame-header "") (parse-frame ""))
(test '(error invalid-frame "\x00\x01") (parse-frame "\x00\x01"))

(test
 "parse list-get-response ok"
 `(frame 3 (list-get-response FS_OK ("EPG Cambridge   \x00")))
 (parse-frame "\x00\x03\x81\x00\x00\x00\x00\x01\x01\x00\x11EPG Cambridge   \x002"))

(test
 "parse list-get-response fail"
 `(frame 3 (list-get-response FS_FAIL "\231"))
 (parse-frame "\x00\x03\x81\205\231")) ;; \xef : magic checksum

(test-group
 "parse-status-codes"
 (test '(FS_FAIL FS_OK FS_FAIL)       (unpack-status-codes 3 "\x85\x00\x85"))
 (test '(FS_OK)                       (unpack-status-codes 1 "\x00"))
 (test '(FS_LIST_END)                 (unpack-status-codes 1 "\x01"))
 (test-error                          (unpack-status-codes 2 "\x00"))
 (test '((error expected-eof "\x00")) (unpack-status-codes 0 (->bitstring "\x00"))))

(test "set-itemnotify-response"
      '(frame 1 (item-setnotify-response FS_OK))
      (parse-frame "\x00\x01\x86\x01\x00"))


;;; ********** testing serialization

(test
 "set dab-state on"
 (blob 04          ;; itemset
       01          ;; item-count
       02 01 00 00 ;; node address (dab_state)
       00 01       ;; count
       01          ;; on/off
       )
 (bitstring->blob (dab.state #t)))

(test
 "set dab-state off"
 (blob 04          ;; itemset
       01          ;; item-count
       02 01 00 00 ;; node address (dab_state)
       00 01       ;; count
       00          ;; on/off
       )
 (bitstring->blob (dab.state #f)))

(test
 "get dab-state"
 (blob 00          ;; itemget
       01          ;; item-count
       02 01 00 00 ;; node address (dab_state)
       )
 (bitstring->blob (dab.state)))

(test
 "set dab-scan-state on"
 (blob 04          ;; itemset
       01          ;; item-count
       02 0a 01 00 ;; node address (dab_scan_state)
       00 01       ;; count (bytes)
       01          ;; on/off
       )
 (bitstring->blob (dab.scan.state #t)))

(test
 "set dab-scan-state off"
 (blob 04          ;; itemset
       01          ;; item-count
       02 0a 01 00 ;; node address (dab_scan_state)
       00 01       ;; count (bytes)
       00          ;; on/off
       )
 (bitstring->blob (dab.scan.state #f)))


(test
 "set get-list-item"
 (blob 01          ;; cmd / type
       02 10 0d 00 ;; node-address
       00 00 00 17 ;; index
       02          ;; field-count
       04          ;; short-field
       05          ;; long-field
       00          ;; filter-count
       00          ;; sort-count
       )
 (bitstring->blob (dab.sl.uService #x0017)))

(test
 "set dab-station"
 (blob 04          ;; type: item-set
       01          ;; number of items
       02 10 01 00 ;; node-address
       00 04       ;; payload-length
       00 00 00 17 ;; payload #x17 => 23
       )
 (bitstring->blob (dab.station 23)))

(test
 "item-setnotify udls on"
 (blob 06          ;; item_setnotify
       01          ;; num notifications
       02 11 00 00 ;; node address
       01          ;; on / off
       )
 (bitstring->blob (dab.udls #:notify #t)))

(test
 "item-setnotify udls off"
 (blob 06          ;; item_setnotify
       01          ;; num notifications
       02 11 00 00 ;; node address
       00          ;; on / off
       )
 (bitstring->blob (dab.udls #:notify #f)))

(test
 "item-setnotify tune-status on"
 (blob 06          ;; item_setnotify
       01          ;; num notifications
       02 06 00 00 ;; node address
       01          ;; on / off
       )
 (bitstring->blob (dab.tune.status #:notify #t)))


(test-exit)