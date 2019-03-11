(define-syntax module*
  (syntax-rules ()
    ((module* name export body ...)
     (begin (module name * body ...)
            (import name)))))

(define-syntax comment
  (syntax-rules ()
    ((comment body ...)
     (void))))


(define-syntax module**
  (syntax-rules ()
    ((module** name export body ... )
     (begin body ...))))

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
                nextplay!
                follow!
                player-pause
		player-spotify-unpause
                player-unpause
		player-paused?
                player-pos
		player-duration
                player-seek
                player-quit
                play-command
		spotify-play
		play-addfollower!
		play-rmfollower!
                playing?)

(import chicken scheme data-structures)
(use fmt test uri-common srfi-18 srfi-13 test http-client matchable
     srfi-1 posix
     extras)

(use looper)
(import clojurian-syntax medea)

;; (include "process-cli.scm")
;; (include "concurrent-utils.scm")
(import closing-http-client process-cli concurrent-utils)
(use nanomsg)

;; create shell string for launching `cplay` player daemon. launch it
;; with play!.
(define (cplay source #!key (format #f) (ar #f))
  (assert (number? (or ar 0)))
  (assert (string? (or format "")))
  (let ((lformat (if format (list "-f" format) '()))
        (lsource (list (cond ((uri-reference? source) (uri->string source))
                             (else source))))
        (lar (if ar (list (number->string ar)) '())))
    (append '("play") lformat lsource lar)))

(define (cplay-follower source)
  (let ((lsource (list source "follower")))
    (append '("cplay") lsource)))

(test-group
 "cplay"
 (test '("play" "filename") (cplay "filename"))
 (test '("play" "filename") (cplay (uri-reference "filename")))
 (test '("play" "-f" "alsa" "file") (cplay "file" format: "alsa"))
;; (test '("play" "filename" "follower") (cplay-follower "filename"))
)

;; Convert mixed symbol/string lists to strings for use with cplay CLI interface
(define (symbol-list->string cmd-list)
  (let* ((token-raw (car cmd-list))
         (token-str (if (symbol? token-raw)
	    (symbol->string token-raw)
            token-raw)))
    (if (not (null? (cdr cmd-list)))
      (string-append token-str " " (symbol-list->string (cdr cmd-list)))
      token-str)))

(test-group
 "List decode"
 (test "aa" (symbol-list->string '(aa)))
 (test "aa bb" (symbol-list->string '(aa bb)))
 (test "aa bb cc" (symbol-list->string '(aa bb "cc")))
 (test "a b" (symbol-list->string '("a" b)))
 (test "aa xx bb" (symbol-list->string '(aa "xx" bb)))
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


(define nnsock-gst
  (let ((nnsock-gst (nn-socket 'pair)))
    (nn-connect nnsock-gst "ipc:///data/nanomessage/playmonitor.pair")
    (nn-send nnsock-gst "print_mib\n")
    (print "Before nn-recv on nnsock-gst")
    (print (nn-recv nnsock-gst))
    (print "After nn-recv on nnsock-gst")
    nnsock-gst))

(define (gstplay-cli cmd #!optional (parser values))
  (print "At gstplay-cli:" cmd)
  (nn-send nnsock-gst cmd)
  (let ((response (nn-recv nnsock-gst)))
    (print "Response: " response)
    (if parser
      (parser response))))

(define (play-worker msg)
  (match msg
    (('pos) (gstplay-cli "pos" parse-cplay-pos-response))
    (('duration)
      (call-with-values ;; better way to do this?
      (lambda () (gstplay-cli "pos" parse-cplay-pos-response))
        (lambda (pos #!optional duration) duration)))

    (('paused?) (gstplay-cli "paused?" parse-cplay-paused?-response))
    (('play pcommand on-exit) (gstplay-cli '("play" pcommand)))
    (else (print "-----At playworker " msg))))

(define (prepause-spotify)
  (with-input-from-pipe "spotifyctl 7879 pause" void)
  (thread-sleep! 0.3))

;; Control operations
(define (player-pause)           (play-worker `(pause)))
(define (player-unpause)         (prepause-spotify) (play-worker `(unpause)))
(define (player-spotify-unpause) (play-worker `(unpause)))
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



(define (play! cmd on-exit on-next)
  (prepause-spotify)
  (setup-nexttrack-callback on-next)
  (play-worker `(play ,cmd ,on-exit)))

(define (nextplay! turi on-next)
  (print "At nextplay!:" turi)
  (prepause-spotify)
  (player-nexttrack turi)
  (setup-nexttrack-callback on-next))

(define (play-follower-cmd uid-leader)
  (
   (play-worker `(quit))
   (cplay-follower uid-leader)
   )
  )

(define (follow! ip_leader)
  (prepause-spotify)
  (pp "At follow!")
  (pp ip_leader)
  (play-worker `(play ("play follower " ,ip_leader )(print "# ignoring callback"))))


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
  (print "At player:play-command:" turi)
  (let ((turi (if (uri? turi) turi (uri-reference turi))))
    (case (uri-scheme turi)
      ((tr) (play-command/tr turi))
      (else (cplay turi)))))

(define (next-command turi)
  (car (cdr (play-command turi))))

(define (play-addfollower! uid_follower)    (print "in call") (play-worker `(add , uid_follower)) (print "after call"))

(define (play-rmfollower! uid_follower) (play-worker `(remove, uid_follower)))

(define (spotify-play parameter)
  (play-worker `(play ("play" , "spotify") (print "# ignoring callback"))))

(test-group "play-command"
 (test '("play" "file:///filename") (play-command "file:///filename"))
 (test '("play" "http://domain/file.mp3") (play-command "http://domain/file.mp3"))
 (test '("play" "filename") (play-command "filename"))
 (test-error (play-command "i l l e g a l"))
 (test "filename" (next-command "filename")))

(define nexttrack-callback #f)
(define monitor-thread #f)
(define used-callback-position 0)

(define (do-nexttrack-callback)
  (let ((cb nexttrack-callback))
    (print "CB:" cb)
    (if cb
	(begin
	  (set! nexttrack-callback #f)
	  (cb)))))

(define (monitor-body)
  (let ((pos (player-pos)))
    (if (and pos (< pos 30000000))
      (let ((duration (player-duration)))
        (if (and (< pos used-callback-position)
                 (< pos duration))
          (set! used-callback-position 0))
;;        (print "Monitor-body: " pos " - " duration " p "  used-callback-position )
        (if (and (< (- duration pos) 15)
                 (< pos duration)
                 nexttrack-callback)
          (if (equal? used-callback-position 0)
            (begin
              (set! used-callback-position pos)
              (do-nexttrack-callback)))))
      (print "No player"))))

(define (make-monitor-thread)
  (thread-start!
    (->>
      monitor-body
      (loop/interval 4)
      (loop/exceptions (lambda (e) (pp `(error: ,(current-thread)
                                           ,(condition->list e))) #t))
      (loop)
      ((flip make-thread) "Monitor") )))

(define (setup-nexttrack-callback on-next)
  (set! nexttrack-callback on-next)
  (if (or (not monitor-thread)
          (not (thread? monitor-thread))
          (equal? (thread-state monitor-thread) 'terminated )
          (equal? (thread-state monitor-thread) 'dead ))
      (set! monitor-thread (make-monitor-thread))))

;; Start gstplayer
(play-worker `(start))

)
