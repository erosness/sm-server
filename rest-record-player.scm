;;; cube-server record-player:

;;(use irregex matchable nanoif)
(import restlib turi)

;; Some static variables keeping track of the availability of the record
;; player.
(define rp-turi #f)
(define rp-subaddr #f)

;; ======================= Bluetooth REST interface ====================
;;
(define-handler /v1/catalog/record-player
  (lambda ()
    (if rp-turi
    `((turi .     ,rp-turi)
      (title .    "IXION Symphony")
      (subtitle . "Record Player")
      (type .     "record-player"))
    (response-unavailable))))

;; ======================= Detect sources ==============================
;; Brute-force detection of record-player: just ask everyone.
;; We assume the PLC network runs the usual 192.168.42.0/28 net. There is a tiny
;; possibility this is not the case. However, this brute-force method should be
;; replaced quite soon.
(use medea http-client clojurian-syntax looper)

;; Try to get icon etc. from record-player. Spends ~10s timing out and returns
;; false if there is no record-player at subaddress.
(define (fetch-source-turi subaddr)
  (let ((http-request (conc "http://192.168.42." (number->string subaddr) ":5055/v1/source/icon")))
    (handle-exceptions x #f
      (receive (payload-string uri response)
        (with-input-from-request http-request #f read-string)
        (alist-ref 'turi (read-json payload-string))))))

;; Update rp-subaddress and rp-turi based on request to one subaddress.
(define (*fetch-source-turi subaddr)
  (let ((addr rp-subaddr)
        (turi rp-turi)
        (current (fetch-source-turi subaddr))) ;; This call takes some time.
    (if (and turi (not current) addr (= subaddr addr))
      (begin
        (set! rp-turi #f)
        (set! rp-subaddr #f)))
    (if (and current (not turi))
      (begin
        (set! rp-turi current)
        (set! rp-subaddr subaddr)))))

;; Create one thread for each possible subaddress, each thread updating based on
;; one subaddress
(define (spawn-source-detect-threads)
  (print "Detect:" rp-subaddr " = " rp-turi)
  (do ((subaddr 2 (+ subaddr 1))) ((= subaddr 15))
    (thread-start!
      (make-thread
        (lambda ()
          (*fetch-source-turi subaddr))))))

;; Do update at regular intervals
(define (make-source-detect-thread)
  (thread-start!
    (->>
      spawn-source-detect-threads
      (loop/interval 20) ;; Interval 20s is a best guess value. Keep Interval
                          ;; low for rapid response, keep interval high not to
                          ;; spam with requests. Keep higher than timeout vs.
                          ;; non-existing units, i.e. ~10s.
      (loop)
      ((flip make-thread) "Source detect thread"))))

(define t (make-source-detect-thread))
