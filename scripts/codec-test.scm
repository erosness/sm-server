(use posix irregex fmt clojurian-syntax)

(define files (with-input-from-pipe "adb shell ls /data/test\\*.\\*" read-lines))

;; calculate number of seconds of input-data (grep ffprobe output)
(define (duration line)
  (irregex-fold "Duration: 00:([0-9]{2}):([0-9]{2})"
                (lambda (i m s)
                  (let* ((xtrs (cut irregex-match-substring m <>))
                         (xtr  (o string->number xtrs))
                         (mm (xtr 1)) (ss (xtr 2)))
                    (+ (* 60 mm) ss)))
                #f
                line))

;; (test 4  (duration "Hi there \r\n\r\nDuration: 00:00:04.51\r\n"))
;; (test 64 (duration "Hi there \r\n\r\nDuration: 00:01:04.22\r\n"))

(define (timer proc)
  (let* ((start (current-milliseconds))
         (result (proc)))
    (values result (/ (- (current-milliseconds) start) 1000))))

(define (make-command f) (conc "adb shell ffmpeg -i " f " -f s16le - \\>/dev/null"))

;; play a file into /dev/null and see how long it takes
(define (play f)
  (receive (result time)
      (timer (lambda () (with-input-from-pipe (make-command f) read-string)))
    (list f result time)))



(define (codec-string output)
  (let ((m (irregex-search "Duration: *([^\r]*)\r*\n.*Stream #([^\r]*)\r*\n" output)))
    (conc (irregex-match-substring m 1) ", #" (irregex-match-substring m 2))))

(define (print-res li)
  (let ((filename (car li))
        (alength (duration (second li)))
        (playback-time (caddr li))
        (output (cadr li)))
    (fmt #t
         (->> (columnar 20 (wrap-lines (dsp filename)) " "
                        8 (dsp (/ alength playback-time))
                        8 (dsp playback-time)
                        8 (dsp alength)
                        ";; " (wrap-lines (dsp (codec-string output))))
              (decimal-align 5)
              (fix 1)
              (with-width 100)))))

;; (define results (map play files))
;; (begin (print) (for-each print-res results))


(print "using command: " (make-command "$filename"))


(fmt #t nl (columnar 20 (dsp "file")
                     8  (dsp "speedup")
                     8  (dsp "test")
                     8  (dsp "actual")
                     ";; " (dsp "codec")))

(for-each (o print-res play) files)
