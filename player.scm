;;; player: a simple wrapper around gstplay
;;;
;;; supports tr:// scheme so that PQ and cube-browser can be on
;;; separate machine. some tracks, like wimp's tid's, need to
;;; construct a stream-url based on the track-id. this stream-url is
;;; valid in 30 minutes and generated just before playback. it is the
;;; stream-url that is passed to cplay.
;;;
;;; if the scheme of the turi is http, it is passed to cplay direcly.
;;; if it is tr://, that url is used to generate a stream-url.
;;;
;;; some tr:// requests actually modify the underlying state. eg.
;;; tr://localhost/t2s?type=dab&id=57 would tune the DAB to channel 57
;;; and return a static DAB-url (like
;;; http://localhost:3345/ffmpeg/ALSA_1_DAB)

(module* player (cplay
                play!
                leader-play!
                follow!
                player-pause
                player-unpause
                player-pos
                player-seek
                player-quit
                play-command
		player-follower
		player-addfollower
		player-rmfollower)

(import chicken scheme data-structures)
(use fmt test uri-common srfi-18 test http-client matchable
     srfi-1 posix
     extras ;; <-- pp
     clojurian-syntax medea)

;; (include "process-cli.scm")
;; (include "concurrent-utils.scm")
(import closing-http-client process-cli concurrent-utils)


;; create shell string for launching `cplay` player daemon. launch it
;; with play!.
(define (cplay source #!key (format #f) (ar #f))
  (assert (number? (or ar 0)))
  (assert (string? (or format "")))
  (let ((lformat (if format (list "-f" format) '()))
        (lsource (list (cond ((uri-reference? source) (uri->string source))
                             (else source))))
        (lar (if ar (list "-ar" (number->string ar)) '())))
    (append '("cplay") lformat lar lsource)))

(define (cplay-follower source)
  (let ((lsource (list source "follower")))
    (append '("cplay") lsource)
    )
  )


(test-group
 "cplay"
 (test '("cplay" "filename") (cplay "filename"))
 (test '("cplay" "filename") (cplay (uri-reference "filename")))
 (test '("cplay" "-f" "alsa" "file") (cplay "file" format: "alsa"))
 (test '("cplay" "-f" "device" "-ar" "44100" "file") (cplay "file" format: "device" ar: 44100))
 (test '("cplay" "filename" "follower") (cplay-follower "filename"))
)

;; pos responses from cplay contain both pos and duration. return both
;; here too.
(define (parse-cplay-pos-response resp)
  (match (string-split resp)
    (("ok" pos duration)
     (values (string->number pos)
             (string->number duration)))
    (else (values 0 0))))

(define (parse-add-response resp)
  (print "Response from gstplay: " resp))
(define (parse-remove-response resp)
  (print "Response from gstplay: " resp))
(define (parse-nexttrack?-response resp)
  (print "Nexttrack? response from gstplay: " resp))
(define (parse-nexttrack-response resp)
  (print "Nexttrack set response from gstplay: " resp))


(test-group
 "parse cplay pos"
 (test "parse cplay pos - success"
       '(23.2341 45.23)
       (receive (parse-cplay-pos-response "ok 23.2341 45.23")))

 (test "parse cplay pos - failure"
       '(0 0)
       (receive (parse-cplay-pos-response "some garbage 1234"))))

(define (parse-cplay-paused?-response resp)
  (and-let* ((value (string-split resp))
             ((equal? (length value) 2))
             (value (cadr value))
             ((or (equal? value "false") (equal? value "true"))))
    (equal? value "true")))

(test-group
 "parse-cplay-paused?"
 (test "truthy" #t         (parse-cplay-paused?-response "ok true"))
 (test "falsy"  #f         (parse-cplay-paused?-response "ok false"))
 (test "bad input" #f      (parse-cplay-paused?-response "ok asdf"))
 (test "more bad input" #f (parse-cplay-paused?-response "foo"))
 (test "empty input" #f    (parse-cplay-paused?-response "")))


;; create a worker which can process one message at a time
;; sequentially. this makes the world a simpler place. we should
;; probably introduce this model on all mutation to the player.
(define (make-play-worker)
  (print "entering playworker")
  (define cplay-cmd (lambda a #f))

  (define (send-cmd str #!optional (parser values))
    (let ((response (cplay-cmd str)))
      (if (string? response)
          (parser response)
          #f)))

  (lambda (msg)
    (match msg
      (('play scommand on-exit)
       ;; reset & kill old cplayer
       (cplay-cmd #:on-exit (lambda () (print ";; ignoring callback")))
       (cplay-cmd "quit")
       (print "starting player")
       (set! cplay-cmd
         (process-cli
         (car scommand)
         (cdr scommand)
         (lambda ()
           ;; important: starting another thread for this is like
           ;; "posting" this to be ran in the future. without
           ;; this, we'd start nesting locks and things which we
           ;; don't want.
           (thread-start! on-exit)))))

       (('leader-play scommand on-exit)
       ;; reset & kill old cplayer
       (cplay-cmd #:on-exit (lambda () (print ";; ignoring callback")))
       (cplay-cmd "quit")
       (set! cplay-cmd
         (process-cli
         (car scommand)
         (append (cdr scommand) '("leader"))
         (lambda ()
           (thread-start! on-exit)))))

       (('follow scommand on-exit)
       ;; reset & kill old cplayer
       (cplay-cmd #:on-exit (lambda () (print ";; ignoring callback")))
       (cplay-cmd "quit")
       (set! cplay-cmd
         (process-cli
         (car scommand)
         (append (cdr scommand) '("follower"))
         (lambda ()
           (thread-start! on-exit)))))

      (('pos)      (send-cmd "pos" parse-cplay-pos-response))
      (('duration)
        (call-with-values ;; better way to do this?
        (lambda () (send-cmd "pos" parse-cplay-pos-response))
          (lambda (pos #!optional duration) duration)))
      (('paused?)       (send-cmd "paused?" parse-cplay-paused?-response))
      (('pause)         (send-cmd "pause"))
      (('unpause)       (send-cmd "unpause"))
      (('seek pos)      (send-cmd (conc "seek " pos) parse-cplay-pos-response))
      (('add uid)       (send-cmd (conc "add " uid) parse-add-response) )
      (('remove uid)    (send-cmd (conc "remove " uid) parse-remove-response) )
      (('nexttrack nxt) (send-cmd (conc "nexttrack " nxt) parse-nexttrack-response) )
      (('nexttrack?)    (send-cmd "nexttrack?" parse-nexttrack?-response) )
      (('quit)          (send-cmd "quit"))
      (else (print "Unknown command: " msg)))) )

(define play-worker
  (let ((mx (make-mutex)))
    (with-mutex-lock mx (make-play-worker))))

(define (prepause-spotify)
  (with-input-from-pipe "spotifyctl 7879 pause" void)
  (thread-sleep! 0.1))

;; Control operations
(define (player-pause)           (play-worker `(pause)))
(define (player-unpause)         (prepause-spotify) (play-worker `(unpause)))
(define (player-paused?)         (play-worker `(paused?)))
(define (player-pos)             (play-worker `(pos)))
(define (player-duration)        (play-worker `(duration)))
(define (player-seek seek)       (prepause-spotify) (play-worker `(seek ,seek)))
(define (player-quit)            (play-worker `(quit)))
;; cplay running and not paused:
(define (playing?)   (and (not (eq? #f (play-worker `(pos))))
                          (not (player-paused?))))
(define (player-nexttrack?) (play-worker `(nexttrack?)))

(define (player-nexttrack turi)
  (let ((nxt  (next-command turi)))
    (play-worker `(nexttrack ,nxt))))

(define (play! cmd on-exit)
  (prepause-spotify)
  (play-worker `(play ,cmd ,on-exit)))

(define (play-follower-cmd uid-leader)
  (
   (play-worker `(quit))
   (cplay-follower uid-leader)
   )
  )

(define (leader-play! cmd on-exit)
  (prepause-spotify)
  (pp "At leader-play!") ;; ?????
  (pp cmd) ;; ?????
  (play-worker `(leader-play ,cmd ,on-exit)))

(define (follow! cmd on-exit)
  (prepause-spotify)
  (pp "At follow!") ;; ?????
  (pp cmd) ;; ?????
  (play-worker `(follow ,cmd ,on-exit)))


(define (play-command/tr turi)
  (let ((response (with-input-from-request* (update-uri turi
                                                        scheme: 'http
                                                        port: (uri-port turi))
                                            #f
                                            read-json)))
    (cplay (alist-ref 'url response)
           format: (alist-ref 'format response)
           ar: (alist-ref 'ar response))))


(define (play-command turi)
  (let ((turi (if (uri? turi) turi (uri-reference turi))))
    (case (uri-scheme turi)
      ((tr) (play-command/tr turi))
      (else (cplay turi)))))

(define (next-command turi)
  (car (cdr (play-command turi))))

(define (play-addfollower uid_follower)    (print "in call") (play-worker `(add , uid_follower)) (print "after call"))

(define (play-rmfollower uid_follower) (play-worker `(remove, uid_follower)))

(define (play-follower uid_leader)
  (play-worker `(play ("cplay" ,uid_leader "follower")   (print ";; ignoring callback")))
  )

(test-group "play-command"

 (test '("cplay" "file:///filename") (play-command "file:///filename"))
 (test '("cplay" "http://domain/file.mp3") (play-command "http://domain/file.mp3"))
 (test '("cplay" "filename") (play-command "filename"))
 (test-error (play-command "i l l e g a l")))
 (test "filename" (next-command "filename"))
)


