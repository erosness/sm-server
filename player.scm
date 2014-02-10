(use fmt)

(define (ffmpeg-command uri)
  (fmt #f "ffmpeg -i \""
       (slashified (uri->string uri))
       "\" -ac 2 -ar 44100 -f s16le -"))

(define (aplay) "aplay -f s16_le -c 2 -r 44100")
(define (pipe a b) (conc a " | " b))

(define (play! command)
  (define-values (*play-ip* *play-op* *play-pid* *play-ep*)
    (process* (pipe command (aplay))))
  ;(values (lambda () (display "q" *play-op*)))
  *play-ep*)

;; TODO propagate stderr from subprocess
;; TODO generic interface for stop playback (and maybe seek etc)

;; (define ffmpeg-exit (play! (ffmpeg "/home/klm/music/The Beatles/The Beatles - Yesterday.mp3")))
(define (tone-command uri)
 (let ((hz (second (uri-path uri))))
   (conc "tone-generator " hz " 30000 1")))

(define audio-hosts
  (make-parameter `(("tone" . ,tone-command))))

(define default-command ffmpeg-command)

(define play-command
  (lambda (turi) ;; <-- string
    (let ((uri (uri-reference turi)))
      (if (and uri (eq? (uri-scheme uri) 'tr))
          ;; find a match among all audio hosts
          (or (any (lambda (pair)
                     (if (equal? (uri-host uri) (car pair))
                         ((cdr pair) uri)
                         #f))
                   (audio-hosts))
              (error (conc "unknown host: " (uri-host uri))))
          (error (conc "don't know how to open " turi))))))


(test "tone-generator 1234 30000 1" (play-command "tr://tone/1234"))

(test-error (play-command "filename"))
(test-error (play-command "file:///filename"))
(test-error (play-command "i l l e g a l"))

;; (define stop (play! (play-command "tone://sine/440")))
;; (read-string #f stop)
;; (stop)

