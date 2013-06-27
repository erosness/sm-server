;;; part of dsp module
;;; safeload write parameters to dsp
;;; safeloaded parameters take affect at the end of the audio-sample
;;; and, unlike writing to RAM directly, won't cause clutching noises

(use srfi-1 srfi-4 blobbery bitstring clojurian-syntax)

;; see adau1701 datasheet, page 32 for details
;;                             data0  adr0    (initiate safeload transfer, p.39)
(define *adau1701-safeloads* `(#x0810 #x0815 ,(blob-string 08 1C 00 3C)))

;; data is payload (must be 4 bytes)
(define (write-safe-param data4 safeload-data0 index)  
  (-> (bitconstruct ((+ safeload-data0 index) 16)
                    (0 8)
                    (data4 32 bitstring))
      (bitstring->blob)
      (blob->string)))

;; param-adr is the address of the audio component from Sigma Studio
(define (write-safe-adr param-adr safeload-address0 index)
  (assert (number? param-adr))
  (-> (bitconstruct ((+ safeload-address0 index) 16)
                    (param-adr 16))
      (bitstring->blob)
      (blob->string)))


(define (safeload adata #!optional (safeloads *adau1701-safeloads*))
  (assert (= 3 (length safeloads)))
  (let ((safeload-data0 (first safeloads))
        (safeload-adr0  (second safeloads))
        (safeload-ist (third safeloads)))
    (assert (string? safeload-ist))
    (append
     ;; fold over (param_adr . payload) pairs, making two packets for
     ;; each pair
     (let loop ((adata adata)
                (index 0)
                (result '()))
       (if (pair? adata)
           (let ((pair (car adata))
                 (rest (cdr adata)))
             (let ((param-adr (car pair))
                   (payload   (cdr pair)))
               (loop rest
                     (add1 index)
                     (append result
                             (list (write-safe-param payload safeload-data0 index)
                                   (write-safe-adr param-adr safeload-adr0 index))))))
           result))
     ;; last packet (perform transfer):
     (list safeload-ist))))




