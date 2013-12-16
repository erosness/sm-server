(use bitstring)

(define ($frame fid body)
  (bitconstruct (fid 16)
                (body bitstring)))

(define ($list-get list-item)
  (bitconstruct (#x01 8) ;; header
                (list-item bitstring)))

;; TODO: support arbitrary number of fields?
(define ($dab-sl-uservice index field1 field2)
  (bitconstruct ("\x02\x10\x0d\x00" bitstring) ;; node address
                (index 32)                     ;;
                (2 8)                          ;; field-count
                (field1 bitstring)             ;;
                (field2 bitstring)             ;;
                (0 8)                          ;; filter-count
                (0 8)                          ;; sort-count
                ))

(define ($field-label)       (bitconstruct (#x04 8)))
(define ($field-short-label) (bitconstruct (#x05 8)))

(define (dab-get-uservice fid uservice-index)
  ($frame fid ($list-get ($dab-sl-uservice uservice-index
                                        ($field-label)
                                        ($field-short-label)))))



(define ($item-set item)
  (bitconstruct
   (#x04 8) ;; type
   (#x01 8) ;; number of items
   (item bitstring)))

(define ($dab-station channel)
  (bitconstruct ("\x02\x10\x01\x00" bitstring) ;; node address
                (#x0004 16)                    ;; payload length
                (channel 32)                   ;; payload
                ))

(define (dab-set-station fid channel)
  ($frame fid ($item-set ($dab-station channel))))

(define ($dab-state on?)
  (bitconstruct ((symbol->node-address 'state) 32)
                (#x001 16)
                ((if on? 1 0) 8)))

(define (dab-set-state fid on?)
  ($frame fid ($item-set ($dab-state on?))))

(define (dab-set-scan-state fid on?)
  ($frame fid
          ($item-set
           (bitconstruct
            ((symbol->node-address 'scan-state) 32)
            (#x001 16) ;; payload length in bytes
            ((if on? 1 0) 8))) ))

;; 3.3.5
(define ($item-setnotify adr on?)
  (bitconstruct
   (#x06 8)   ;; type (item_setnotify
   (1 8)      ;; number of nodes
   (adr 32)   ;; node address
   ((if on? 1 0) 8)))

(define (dab-set-udls fid on?)
  ($frame fid ($item-setnotify (symbol->node-address 'udls) on?) ))

(define (dab-set-tune-status fid on?)
  ($frame fid ($item-setnotify (symbol->node-address 'tune-status) on?) ))
