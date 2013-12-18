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

(define (dab.sl.uService uservice-index)
  ($list-get ($dab-sl-uservice uservice-index
                               ($field-label)
                               ($field-short-label))))



(define ($item-set item)
  (bitconstruct
   (#x04 8) ;; type
   (#x01 8) ;; number of items
   (item bitstring)))

(define ($item-get item)
  (bitconstruct
   (#x00 8) ;; type
   (#x01 8) ;; number of items
   (item bitstring) ;; node address
   ))

(define dab.station
  (case-lambda
    ((channel)
     ($item-set
      (bitconstruct ("\x02\x10\x01\x00" bitstring) ;; node address
                    (#x0004 16) ;; payload length
                    (channel 32) ;; payload
                    )))))

(define ($dab-state on?)
  (bitconstruct ((symbol->node-address 'state) 32)
                (#x001 16)
                ((if on? 1 0) 8)))

(define dab.state
  (case-lambda
    (()    ($item-get "\x02\x01\x00\x00"))
    ((on?) ($item-set ($dab-state on?)))))


(define dab.scan.state
  (case-lambda
    (() ($item-get "\x02\x0a\x01\x00"))
    ((on?)
     ($item-set
      (bitconstruct
       ("\x02\x0a\x01\x00" bitstring)
       (#x001 16) ;; payload length in bytes
       ((if on? 1 0) 8))))))

;; 3.3.5
(define ($item-setnotify adr on?)
  (bitconstruct
   (#x06 8)   ;; type (item_setnotify
   (1 8)      ;; number of nodes
   (adr bitstring)   ;; node address
   ((if on? 1 0) 8)))

;; R/O N
(define dab.udls
  (case-lambda
    (()    ($item-get       "\x02\x11\x00\x00"))
    ((on?) ($item-setnotify "\x02\x11\x00\x00" on?))))

;; R/O N
(define dab.tune.status
  (case-lambda
    (()    ($item-get       "\x02\x06\x00\x00"))
    ((on?) ($item-setnotify "\x02\x06\x00\x00" on?))))


(define (misc.clock.localTime)
  ($item-get "\x06\x01\x01\x00"))


(define (audio.sampleRate)
  ($item-get "\x05\x03\x00\x00"))


;;; ================================================== FM

;; e8
(define fm.state
  (case-lambda
    (()    ($item-get "\x03\x01\x00\x00"))
    ((on?) ($item-set
            (bitconstruct ("\x03\x01\x00\x00" bitstring) ;; addr
                          (#x0001 16) ;; payload length
                          ((if on? 1 0) 8))))))

;; u32 N
(define fm.frequency
  (case-lambda
    (()     ($item-get "\x03\x03\x00\x00"))
    ((freq) ($item-set "\x03\x03\x00\x00"))))

;; e8 R/O N
(define fm.tuneStatus
  (case-lambda
    (() ($item-get "\x03\x08\x00\x00"))))
