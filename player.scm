(use fmt)

(define (ffmpeg source)
  (fmt #f "ffmpeg -i \""
       (slashified source)
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

(define play-command
  (let ((player #f))
    (lambda (turi)
      (let ((uri (uri-reference turi)))
        (case (and uri (uri-scheme uri))
          ((tone) (let ((hz (second (uri-path uri))))
                    (conc "./tone-generator.scm " hz " 30000 1")))
          (else (ffmpeg turi)))))))

(test "ffmpeg -i \"filename with spaces.mp3\" -ac 2 -ar 44100 -f s16le -"
      (play-command "filename with spaces.mp3"))

;; (define stop (play! (play-command "tone://sine/440")))
;; (read-string #f stop)
;; (stop)

