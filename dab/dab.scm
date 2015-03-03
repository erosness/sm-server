(module dab *
(import extras chicken scheme)


(use s48-modules)


(include-relative "frame-parse.scm")
(include-relative "frame-serialize.scm")

(use matchable test bitstring srfi-18 srfi-14 srfi-13)

(define (dab-command data) (error "not yet implemented. (use dab-nn) or something."))

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
 (define (parse-dab.sl.uService data)
   (match data
     (('list-get-response 'FS_OK (channel . rest))
      (string-trim-both channel (char-set #\space #\newline #\nul)))
     (('list-get-response 'FS_FAIL "") #f)
     (else (error "invalid dab.sl.uService response" data))))

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

 (define (dab-channel-name idx)
   (parse-dab.sl.uService (dab-command (dab.sl.uService idx))))

 ;; list all channels (idx . "label"). this is slow.
 (define (dab-channels*)
   (let loop ((n 1)
              (res '()))
     (let ((channel (dab-channel-name n)))
       (if channel
           (loop (add1 n)
                 (cons (cons n channel) res))
           (reverse res)))))

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
