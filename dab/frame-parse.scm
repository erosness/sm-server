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
;; has only 27-bit for fixnums. As long as all nodes are <= #x03FFFFFF
;; then we'll be fine.
(define (node-address->symbol adr)
  (case adr
    ((#x02010000) 'state)
    ((#x020a0100) 'scan_state)
    ((#x020a0200) 'scan_update)
    ((#x02060000) 'tunestatus)
    ((#x02110000) 'udls)
    ((#x02100d00) 'sl_uService_list)
    ((#x02100100) 'sl_station)
    (else adr)))


(define (parse-notification body)
  (bitmatch body
            ( ((address 32) (payload bitstring))
              `(notification ,(node-address->symbol address)
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
