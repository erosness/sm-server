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
(use fmt test uri-common srfi-18 test http-client
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

(define *cplay-lock* (make-mutex))
(define *cplay-proc* #f)

;; spawn command, killing the previous one if it's running
;; TODO: support on-exit callback
(define play!
  (with-mutex-lock
   *cplay-lock*
   (lambda (scommand #!optional (on-exit (lambda () (print "*** song finished"))))
     (print scommand)
     (if *cplay-proc* (handle-exceptions e
                        (pp (condition->list e))
                        (*cplay-proc* #:on-exit (lambda () (print ";; callback cleared by play!")))
                        (if (port-closed? (*cplay-proc* #:stdout))
                            (void) ;; already quit
                            (*cplay-proc* "quit")))) ;; <-- clean cplay exit

     (set! *cplay-proc* (process-cli (car scommand)
                                     (cdr scommand)
                                     on-exit))
     *cplay-proc*)))


(define (parse-cplay-pos-response resp)
  (and-let* ((l (drop (string-split resp) 1))
         (pos (string->number (car l)))
         (total (string->number (cadr l))))
    `((pos . ,pos)
      (total . ,total))))

(test "parse cplay pos - success"
      '((pos . 23.2341)
        (total . 45.23))
      (parse-cplay-pos-response "ok 23.2341 45.23"))

(test "parse cplay pos - failure"
      #f (parse-cplay-pos-response "some garbage 1234"))

(define (parse-cplay-paused?-response resp)
  (and-let* ((value (string-split resp))
             ((equal? (length value) 2))
             (value (cadr value))
             ((or (equal? value "false") (equal? value "true"))))
    `((value . ,(equal? value "true")))))

(test-group
 "parse-cplay-paused?"
 (test "truthy" `((value . #t)) (parse-cplay-paused?-response "ok true"))
 (test "falsy"  `((value . #f)) (parse-cplay-paused?-response "ok false"))
 (test "bad input" #f (parse-cplay-paused?-response "ok asdf"))
 (test "more bad input" #f (parse-cplay-paused?-response "foo"))
 (test "empty input" #f (parse-cplay-paused?-response "")))


(define (player-operation op #!rest args)
  (with-mutex-lock
   *cplay-lock* (lambda () (and *cplay-proc* (apply *cplay-proc* op args)))))

;; Control operations
(define player-pause
 (player-operation "pause"))

(define player-unpause
 (player-operation "unpause"))

(define (player-paused?)
  (cond ( ((player-operation "paused?")) => parse-cplay-paused?-response)))

(define (player-pos)
  (cond ( ((player-operation "pos")) => parse-cplay-pos-response)))

(define (player-seek seek)
  ((player-operation "seek" (->string seek))))

(define player-quit
  (player-operation "quit"))

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
