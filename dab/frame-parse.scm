(use bitstring)

(define (integer->status-code int)
  (case int
    ((#x00) 'ok)
    ((#x85) 'fail)
    (else int)))

(define (unpack-status-codes num bs)
  (if (> num 0)
      (bitmatch bs
                (((sc 8) (rest bitstring))
                 (cons (integer->status-code sc)
                       (unpack-status-codes (sub1 num) rest))))
      (if (= 0 (bitstring-length bs))
          '()
          (error "expecting empty bitstring" bs))))

(define (parse-item-get-responses num bs)
  (if (> num 0)
      (bitmatch bs
                ( ((sc 8)
                   (data-length 16)
                   (body (* 8 data-length) bitstring)
                   (rest bitstring))
                  (cons (cons (integer->status-code sc)
                              (blob->string (bitstring->blob body)))
                        (parse-item-get-responses (sub1 num) rest)))
                )
      (if (= 0 (bitstring-length bs))
          '()
          (error "expecting empty bitstring" bs))))

;; FSAPI protocol reference 3.4.5
(define (parse-fields num bs)
  (if (> num 0)
      (bitmatch bs
                ( ((field-length 16)
                   (field-data (* 8 field-length) bitstring)
                   (rest bitstring))
                  (cons (blob->string (bitstring->blob field-data))
                        (parse-fields (sub1 num) rest))))
      '()))

(define (parse-list-get-responses  bs)
  (bitmatch bs
            ( ((sc 8)
               (key 32)
               (num-fields 8)
               (rest bitstring))
              `(,(integer->status-code sc) ,(parse-fields num-fields rest)))
            ( (( sc 8) (rest bitstring))
              `(,(integer->status-code sc) ,(blob->string (bitstring->blob rest))))))



;; OBS! Nodes are 32-bit addresses, but Chicken, on a 32-bit system,
;; has only 27-bit for fixnums. As long as all nodes are <=
;; #x03FFFFFF (most-positive-fixnum), then we'll be fine
(define node-addresses
  '((state            . #x02010000)
    (scan-state       . #x020a0100)
    (scan-update      . #x020a0200)
    (tune-status       . #x02060000)
    (udls             . #x02110000)
    (sl-uservice-list . #x02100d00)
    (sl-station       . #x02100100)))

;; if not found, return integer 
(define (symbol->node-address label)
  (alist-ref label node-addresses))

(define (describe-node adr)
  (define (swap pair) (cons (cdr pair) (car pair)))
  (alist-ref adr (map swap node-addresses) eq? adr))



(define (parse-notification body)
  (bitmatch body
            ( ((address 32) (payload bitstring))
              `(notification ,(describe-node address)
                             ,(blob->string (bitstring->blob payload)) ))))

(define (parse-command bs)
  (bitmatch bs
            ( ((#x84 8) (num-responses 8)
               (responses (* 8 num-responses) bitstring))
              ;; call handler procedure:
              `(item-set-response ,@(unpack-status-codes num-responses responses)))

            ( ((#x80 8) (num-responses 8)
               (responses bitstring))
              `(item-get-response ,@(parse-item-get-responses num-responses responses)))

            ;; TODO: #x83 item-set-response

            ( ((#x81 8) (rest bitstring))
              `(list-get-response ,@(parse-list-get-responses rest)))

            ( ((#xc6 8) (notification-body bitstring))
              `,(parse-notification notification-body))

            ( ((command-type 8) (rest bitstring))
              `(unknown ,command-type ,(blob->string (bitstring->blob rest))))

            ))

(define (parse-frame data)
  (bitmatch data
            ;; TODO: check crc!
            ( ((fid 16) (rest bitstring))
              `(frame ,fid ,(parse-command rest)))

            (else (error "expected 16-bit fid and 8-bit type"
                         (bitstring->blob (bitstring-of-any data)))))  )
