(use srfi-4)

;; Scheme implementation of SLIP (http://tools.ietf.org/html/rfc1055)
;; The reference implementation was not used because it did not allow
;; signalling a full buffer, neither did it handle EOF.
;; We only need the byte-stream --> packet conversion which is
;; implemented below.

(define (read-slip #!optional (port (current-input-port)))
  (define END     (integer->char #o300))
  (define ESC     (integer->char #o333))
  (define ESC_END (integer->char #o334))
  (define ESC_ESC (integer->char #o335))
  ;; read from port recursively until we hit END or #!eof
  ;; if we END with empty packet, we just continue.
  ;; hitting eof with empty packet, we return #f to signal eof.
  ;; hitting eof with non-empty packet, we fail (illegal packet)
  (let loop ((r '()))
    (let ((c (read-char port)))
      (if (eof-object? c)
          (if (null? r) #f ;; <-- signal eof
              (error "premature eof while reading packet" (apply string (reverse r))))
          (if (eq? END c)
              (if (null? r)
                  (loop r) ;; <-- drop empty packets
                  (apply string (reverse r)))
              (loop (cons
                     (if (eq? ESC c)
                         (let ((c2 (read-char port)))
                           (if (or (eq? c2 ESC_END) (eq? c2 ESC_ESC))
                               c2 ;; return escaped char
                               (error (format "invalid escape token #o~o in input" (char->integer c2)))))
                         c)
                     r)))))))



(define port (open-input-file "/home/klm/cube/cs/dab-uart-lib/data/channel.uart"))
(parse-frame (let ((i (with-input-from-port port (lambda () (read-slip)))))
               (set! x i) i))


(define (readable? fd)
  (let-values (((rd wr) (file-select fd #f 0))) rd))
(define (writable? fd)
  (let-values (((rd wr) (file-select #f fd 0))) wr))


(define dab-fd (file-open "/dev/ttymxc0" open/rdwr))

(readable? dab-fd)
(writable? dab-fd)

(file-write dab-fd "\300\x00\x01\x04\x01\x02\x10\x01\x00\x00\x04\x00\x00\x00\x01\357\357\300")
(file-read dab-fd 1)

(define tmp-fd (file-open "/cache/tmp" (+ open/rdwr open/creat)))
(file-write tmp-fd "hello")
(set! (file-position tmp-fd) 1)
(file-read  tmp-fd 3)
(file-select tmp-fd #f 0)




(define frame (blob->string (bitstring->blob (dab-set-station 1 1))))
