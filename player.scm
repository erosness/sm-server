(use fmt test uri-common)


(define (cplay-command uri #!optional seek)
  (conc "cplay "
        "\""
        (uri->string uri)
        "\""))
(define (play! command)
  (define-values (*play-ip* *play-op* *play-pid* *play-ep*)
    (process* (pipe command (aplay))))
  ;(values (lambda () (display "q" *play-op*)))
  *play-ep*)

;; TODO propagate stderr from subprocess
;; TODO generic interface for stop playback (and maybe seek etc)

;; (define ffmpeg-exit (play! (ffmpeg "/home/klm/music/The Beatles/The Beatles - Yesterday.mp3")))
(define (play-command/tone uri)
 (let ((hz (second (uri-path uri))))
   (conc "tone-generator " hz " 30000 1")))

;; (audio-hosts)
(define *audio-hosts*
  `(("tone" . ,play-command/tone)
    ("wimp" . ,play-command/wimp)))

(define play-command/default cplay-command)

(define (play-command turi #| <-- string |#)

  (let ((uri (uri-reference turi)))
    (or
     (and uri
          (case (uri-scheme uri)
            ((tr) ;; find a match among all audio hosts
             (or (any (lambda (pair)
                        (if (equal? (uri-host uri) (car pair))
                            ((cdr pair) uri)
                            #f))
                      *audio-hosts*)
                 (error (conc "unknown audio host: " (uri-host uri)))))
            ((file) (cplay-command uri))
            (else #f)))
     (error (conc "don't know how to open " turi)))))



;; (play! (play-command "tr://wimp/tid/12345"))


(test-group
 "play-command"

 (test "tone-generator 1234 30000 1" (play-command "tr://tone/1234"))
 (test "cplay \"file:///filename\"" (play-command "file:///filename"))

 (test-error (play-command "filename"))
 (test-error (play-command "i l l e g a l")))

;; (define stop (play! (play-command "tone://sine/440")))
;; (read-string #f stop)
;; (stop)
