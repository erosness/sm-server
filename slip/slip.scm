(module slip (slip-read
              slip-write)

(import chicken scheme)
(use srfi-4   ;; homogeneous lists
     srfi-13) ;; strings

;; Scheme implementation of SLIP (http://tools.ietf.org/html/rfc1055)
;; The reference implementation was not used because it did not allow
;; signalling a full buffer, neither did it handle EOF.
;; We only need the byte-stream --> packet conversion which is
;; implemented below.
(define END     (integer->char #o300))
(define ESC     (integer->char #o333))
(define ESC_END (integer->char #o334))
(define ESC_ESC (integer->char #o335))

(define (slip-read #!optional (port (current-input-port)))
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

;; TODO: write slip-write

)
