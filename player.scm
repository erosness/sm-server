;;; player: a simple wrapper around cplay
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
                player-pause
                player-unpause
                player-pos
                player-seek
                player-quit
                play-command)

(import chicken scheme data-structures)
(use fmt test uri-common srfi-18 test http-client matchable
     srfi-1
     extras ;; <-- pp
     clojurian-syntax medea)

;; (include "process-cli.scm")
;; (include "concurrent-utils.scm")
(import closing-http-client process-cli concurrent-utils)


;; create shell string for launching `cplay` player daemon. launch it
;; with play!.
(define (cplay source #!optional (format #f))
  (let ((lformat (if format (list "-f" format) '()))
        (lsource (list (cond ((uri-reference? source) (uri->string source))
                             (else source)))))
    (append '("cplay") lformat lsource)))

(test-group
 "cplay"
 (test '("cplay" "filename") (cplay "filename"))
 (test '("cplay" "filename") (cplay (uri-reference "filename")))
 (test '("cplay" "-f" "alsa" "file") (cplay "file" "alsa")))

(define (parse-cplay-pos-response resp)
  (and-let* ((l (drop (string-split resp) 1))
         (pos (string->number (car l)))
         (duration (string->number (cadr l))))
    `((pos . ,pos)
      (duration . ,duration))))

(test "parse cplay pos - success"
      '((pos . 23.2341)
        (duration . 45.23))
      (parse-cplay-pos-response "ok 23.2341 45.23"))

(test "parse cplay pos - failure"
      #f (parse-cplay-pos-response "some garbage 1234"))

(define (parse-cplay-paused?-response resp)
  (and-let* ((value (string-split resp))
             ((equal? (length value) 2))
             (value (cadr value))
             ((or (equal? value "false") (equal? value "true"))))
    `((paused . ,(equal? value "true")))))

(test-group
 "parse-cplay-paused?"
 (test "truthy" `((paused . #t)) (parse-cplay-paused?-response "ok true"))
 (test "falsy"  `((paused . #f)) (parse-cplay-paused?-response "ok false"))
 (test "bad input" #f (parse-cplay-paused?-response "ok asdf"))
 (test "more bad input" #f (parse-cplay-paused?-response "foo"))
 (test "empty input" #f (parse-cplay-paused?-response "")))


;; create a worker which can process one message at a time
;; sequentially. this makes the world a simpler place. we should
;; probably introduce this model on all mutation to the player.
(define (make-play-worker)
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
      (('pos)      (send-cmd "pos" parse-cplay-pos-response))
      (('paused?)  (send-cmd "paused?" parse-cplay-paused?-response))
      (('pause)    (send-cmd "pause" parse-cplay-paused?-response))
      (('unpause)  (send-cmd "unpause" parse-cplay-paused?-response))
      (('seek pos) (send-cmd (conc "seek " pos)))
      (('quit)     (send-cmd "quit")))) )

(define play-worker
  (let ((mx (make-mutex)))
    (with-mutex-lock mx (make-play-worker))))

;; Control operations
(define (player-pause)      (play-worker `(pause)))
(define (player-unpause)    (play-worker `(unpause)))
(define (player-paused?)    (play-worker `(paused?)))
(define (player-pos)        (play-worker `(pos)))
(define (player-seek seek)  (play-worker `(seek ,seek)))
(define (player-quit)       (play-worker `(quit)))
(define (play! cmd on-exit) (play-worker `(play ,cmd ,on-exit)))

(define (play-command/tr turi)
  (let ((response (with-input-from-request* (update-uri turi
                                                        scheme: 'http
                                                        port: (uri-port turi))
                                            #f
                                            read-json)))
    (cplay (alist-ref 'url response)
           (alist-ref 'format response))))

(define (play-command turi)
  (let ((turi (if (uri? turi) turi (uri-reference turi))))
    (case (uri-scheme turi)
      ((tr) (play-command/tr turi))
      (else (cplay turi)))))



(test-group
 "play-command"

 (test '("cplay" "file:///filename") (play-command "file:///filename"))
 (test '("cplay" "http://domain/file.mp3") (play-command "http://domain/file.mp3"))

 (test '("cplay" "filename") (play-command "filename"))
 (test-error (play-command "i l l e g a l")))
)

;; (define player (play! (play-command "tr://localhost:5060/t2s?type=wimp&id=12345678")))
;; (player "quit")
