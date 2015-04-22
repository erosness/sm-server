(module dab *
(import extras chicken scheme)


(use s48-modules)


(include-relative "frame-parse.scm")
(include-relative "frame-serialize.scm")

(use matchable test bitstring srfi-18 srfi-14 srfi-13)
(use nanomsg)

;; ============================== nanomsg interface to fsapi ====================

(define nnsock-dab (nn-socket 'req))
(nn-connect nnsock-dab "tcp://127.0.0.1:12000")


;; mutexless nanomsg-based message passing of binary Venice9/fsapi
;; data. one message is one raw dab packet (with frame idx).
(define (dab-command* frameless)
  (let* ((fid (random #xffff))) ;; for error-checking
    (nn-send nnsock-dab (bitstring->string ($frame fid frameless)))
    (let* ((response (nn-recv nnsock-dab))
           (frame (parse-frame response)))
      (if (string-prefix? "error" response) ;; <-- nndab error blob: "error <msg>"
          (error 'dab-command* response)
          (match frame
            (('frame reply-fid reply)
             (if (not (= fid reply-fid))
                 (error (conc "invalid reply frame for fid " fid) response))
             reply)
            (else (error "invalid DAB frame" response)))))))


;; we need mutexes because it's a req-rep which needs to go in
;; lockstep.
(define dab-command
  (let ((mutex (make-mutex 'dab-command-mutex)))
    (lambda (bs)
      (dynamic-wind
        ;; TODO: handle exceptions here?
        (lambda () (mutex-lock! mutex))
        (lambda () (dab-command* bs))
        (lambda () (mutex-unlock! mutex))))))


;; ==================== DAB utils ====================

 ;; turn off all submodules.
 ;;
 ;; before any module can be turned on (eg dab.state), all the other
 ;; must be off. only one can be on at the same time.
(define (dab-reset)
  (dab-command (dab.state 'off))
  (dab-command (fm.state 'off))
  (dab-command (audio.buzzer.state 'off)))

 ;; turn off everything, then turn dab back on
(define (dab-turn-on)
  (dab-reset)
  (dab-command (dab.state 'on)))

(define (fm-turn-on)
  (dab-reset)
  (dab-command (fm.state 'on)))

(define (parse-dab/fm.state data)
  (match data
    (('item-get-response ('FS_OK . "\x00")) #f)
    (('item-get-response ('FS_OK . "\x01")) 'on)))

(define (dab-on?) (parse-dab/fm.state (dab-command (dab.state))))
(define (fm-on?) (parse-dab/fm.state (dab-command (fm.state))))

 ;; ====================
 ;; TODO: the following should be part of some dab module and i/o
 ;; independent.


 ;; TODO: apply this in all nt:e8 structures instead
(define (parse-dab.scan.state data)
  (match data
    ('(item-get-response (FS_OK . "\x00")) 'idle)
    ('(item-get-response (FS_OK . "\x01")) 'scan)
    ('(item-get-response (FS_NODE_BLOCKED . "")) 'off)
    (else (error "invalid dab.scan.state data" data))))

(test-group
 "parse-dab.scan.state"
 (test 'idle (parse-dab.scan.state '(item-get-response (FS_OK . "\x00"))))
 (test 'scan (parse-dab.scan.state '(item-get-response (FS_OK . "\x01"))))
 (test 'off (parse-dab.scan.state '(item-get-response (FS_NODE_BLOCKED . ""))))

 (test-error (parse-dab.scan.state '(item-get-response (FS_INVALID . "\x00"))))
 (test-error (parse-dab.scan.state '(item (FS_OK . "\x00"))))
 )

 ;; TODO: apply this to all $list-get instead
;; TODO: apply this to all $list-get instead
;; returns `("channel label" service-index "component description")
;; - channel label is the text displayed to user, eg "NRK P2"
;; - service index is an int used to tune to this station (dab.sl.station), eg 22
;; - component description describes contains information for stream type (audio/data) etc
(define (parse-dab.sl.uComponent data)
  (match data
    (('list-get-response 'FS_OK (channel service-key component-description . rest))
     (list (string-trim-both channel (char-set #\space #\newline #\nul))
           (bitmatch service-key (((i 32)) i))
           component-description))
    (('list-get-response 'FS_FAIL "") #f)
    (else (error "invalid dab.sl.uComponent response" data))))


;; TMId (Transport Mechanism Identifier), page 54 of ETSI EN 300 401 V1.4.1
;; this 2-bit field shall indicate the transport mechanism used:
;; bit15 - bit14
;; 0 0: MSC - Stream mode - audio
;; 0 1: MSC - Stream mode - data
;; 1 0: FIDC
;; 1 1: MSC - Packet mode - data
(define (service-component-description-TMId sc)
  ;;                          ,-- two first bits is TMId
  (case (bitmatch sc (((tmid 2) (rest bitstring)) tmid))
    ((0) 'audio)
    ((1) 'data)
    ((2) 'FIDC)
    ((3) 'packet)))

(define (msc-stream-audio? scd) ;; service component description
  (equal? (service-component-description-TMId scd) 'audio))

(test-group
 "tmid"
 (test 'audio  (service-component-description-TMId "\x00\x01"))
 (test 'audio  (service-component-description-TMId "\x3F\x01"))
 (test 'packet (service-component-description-TMId "\300\x01"))
 (test 'FIDC   (service-component-description-TMId "\200\x01"))
 (test 'data   (service-component-description-TMId "\100\x01")))

(define (parse-dab.tuneStatus data)
  (match data
    (('item-get-response ('FS_OK . str))
     (match str
       ("\x02" 'decoding)
       ("\x01" 'tuned)
       ("\x00" 'idle)
       (else (error "no matching " str))))
    (else (error "no matching" data))))

 ;; return dab.sl.station as int. TODO: make generic
(define (parse-dab.sl.station #!optional (data (dab-command (dab.sl.station))))
  (match data
    (('item-get-response (FS_OK . str))
     (bitmatch str (((x 32)) x)))))

 ;; list all channels (idx . "label"). this is slow.
(define (dab-channels*)
  (let loop ((n 1)
             (res '()))
    (let ((channel (parse-dab.sl.uComponent (dab-command (dab.sl.uComponent n)))))
      (match channel
        ((label service-key component-description)
         (if (msc-stream-audio? component-description)
             ;; valid channel, add it
             (loop (add1 n)
                   (cons (cons service-key label) res))
             ;; ignore non-audio-channels
             (loop (add1 n) res)))
        (#f (reverse res)))))) ;; <-- parsing fails, presumably no more channels at index n

 ;; do a full scan. this takes a long time.
(define (dab-full-scan)
  (dab-command (dab.scan.state 'scan))
  (let loop ()
    (print ";; DAB is scanning ...")
    (case (parse-dab.scan.state (dab-command (dab.scan.state)))
      ((scan)
       (thread-sleep! 1)
       (loop))
      (else (void)))))

(define *dab-channels* '())
(define (dab-refresh-channels!)
  (dab-full-scan)
  ;; TODO: thread safety
  (set! *dab-channels* (dab-channels*)))

(define (dab-channels) *dab-channels*)


 ;; ------- FM
(define (parse-fm.frequency data)
  (match data
    (('item-get-response (FS_OK . string))
     (bitmatch string (((x 32)) x)))))

(define (parse-fm.signalStrength data)
  (* -1
     (match data
       (('item-get-response (FS_OK . string))
        (bitmatch string (((x 8)) x))))))

(define (parse-fm.rds.radioText data)
  (match data
    (('item-get-response (FS_OK . channel))
     (and-let* ((trimmed (string-trim-both channel (char-set #\space #\newline #\nul)))
                (dblspace-idx (substring-index "  " trimmed)))
       ;; Return the substring up to the first double space
       (substring trimmed 0 dblspace-idx)))))

(define (parse-fm.search data)
  (match data
    (('item-get-response ('FS_OK . "\x00")) 'idle)
    (('item-get-response ('FS_OK . "\x01")) 'up)
    (('item-get-response ('FS_OK . "\x02")) 'down)
    (('item-set-response 'FS_OK) 'set-ok)
    (else (error "[parse-fm.search] unknown response: " else))))

 ;; API
(define (fm-frequency . hz)
  (if (and (not (null? hz))
           (number? (car hz)))
      (dab-command (fm.frequency (car hz))))

  (parse-fm.frequency
   (dab-command (fm.frequency))))

(define (fm-search . direction)
  (parse-fm.search
   (dab-command (match direction
                  ('() (fm.search))
                  ((or '(up) '(down) '(idle)) (fm.search (car direction)) )
                  (else (error "[fm-search] invalid argument: " else))))))

(define (fm-signal-strength)
  (parse-fm.signalStrength
   (dab-command (fm.signalStrength))))

(define (fm-radio-text)
  (parse-fm.rds.radioText
   (dab-command (fm.rds.radioText))))

(define (fm-tunestatus)
  (parse-dab.tuneStatus (dab-command (fm.tuneStatus))))



)
