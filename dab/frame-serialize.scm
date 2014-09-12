(use bitstring)

;; ============================== helpers

;; getter/setter for single value ($item-set og $item-get) with
;; possible notification.
(define-syntax nt:single
  (syntax-rules ()
    ((_ adr x bcbody ...)
     (case-lambda
       (()             ($item-get adr))
       ((#!key notify) ($item-setnotify adr notify))
       ((x)        ($item-set
                    (bitconstruct (adr bitstring) ;; addr
                                  bcbody ...)))))))

(define-syntax nt:u8
  (syntax-rules ()
    ((_ adr) (nt:single adr x (#x0001 16) (x 8)))))

(define-syntax nt:b8
  (syntax-rules ()
    ((_ adr) (nt:single adr x (#x0001 16) ((if x 1 0) 8)))))

(define-syntax nt:u32
  (syntax-rules ()
    ((_ adr) (nt:single adr x (#x0004 16) (x 32)))))

;; TODO: we don't yet have a serializer for fsapi's c8 types. doing b8
;; instead:
(define-syntax nt:c8
  (syntax-rules ()
    ((_ adr) (nt:single adr x (#x0001 16) (x 8)))))

;; (indexify-list a b c) => '((a . 0) (b . 1) (c . 2))
(define-syntax indexify-list
  (er-macro-transformer
   (lambda (x r t)
     `(,(r 'quasiquote)
       ,(let loop ((lst (cdr x))
                   (idx 0))
          (if (null? lst)
              '()
              (cons (cons (car lst) idx) (loop (cdr lst) (add1 idx)))))))))

(define (alist-ref/error x alist)
  (or (alist-ref x alist) ;; OBS: won't work if #f is in the alist
      (error (conc x " not in enum " (map car alist)))))

(define-syntax nt:e8
  (syntax-rules ()
    ((_ adr enums ...) (nt:single adr x (#x0001 16)
                                  ((alist-ref/error x (indexify-list enums ...)) 8)))))


;; ============================== bitstring constructs

(define ($frame fid body)
  (bitconstruct (fid 16)
                (body bitstring)))

(define ($list-get list-item)
  (bitconstruct (#x01 8) ;; header
                (list-item bitstring)))


;; 3.3.5
(define ($item-setnotify adr on?)
  (bitconstruct
   (#x06 8)   ;; type (item_setnotify
   (1 8)      ;; number of nodes
   (adr bitstring)   ;; node address
   ((if on? 1 0) 8)))


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

;; dab
(define dab.state         (nt:b8  "\x02\x01\x00\x00"))
(define dab.sl.station    (nt:u32 "\x02\x10\x01\x00"))
(define dab.scan.state    (nt:b8  "\x02\x0a\x01\x00"))
(define dab.udls          (nt:c8 "\x02\x11\x00\x00"))
(define dab.tune.status   (nt:b8 "\x02\x06\x00\x00"))

;; fm
(define fm.state           (nt:b8  "\x03\x01\x00\x00"))
(define fm.search          (nt:e8  "\x03\x04\x00\x00" idle up down))
(define fm.signalStrength  (nt:u8  "\x03\x06\x00\x00"))
(define fm.frequency       (nt:u32 "\x03\x03\x00\x00"))
(define fm.tuneStatus      (nt:b8  "\x03\x08\x00\x00"))
(define fm.searchLevel     (nt:e8 "\x03\x05\x00\x00" all strong))

;; fm rds
(define fm.rds.active      (nt:e8  "\x03\x09\x01\x00" idle decoding))
(define fm.rds.ps          (nt:c8  "\x03\x09\x02\x00"))
(define fm.rds.pty         (nt:u8  "\x03\x09\x03\x00"))
(define fm.rds.radioText   (nt:c8  "\x03\x09\x04\x00"))

;; untested
(define (misc.clock.localTime) ($item-get "\x06\x01\x01\x00"))
(define (audio.sampleRate)     ($item-get "\x05\x03\x00\x00"))


