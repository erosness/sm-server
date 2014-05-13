(module player (cplay
                play!
                player-pause
                player-unpause
                player-quit
                play-command)

(import chicken scheme data-structures)
(use fmt test uri-common srfi-18 test)

;; (include "process-cli.scm")
;; (include "concurrent-utils.scm")
(import process-cli concurrent-utils)


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


;; *cplay-proc* calls process-wait which throws an error if the
;; specified child process does not exist, this is expected since some
;; other thread might have already killed it
(define (ignore-wait-for-child-process-failed c)
  (if (not (equal? (get-condition-property c 'exn 'message)
                    "waiting for child process failed - No child processes"))
          (signal c)))

;; spawn command, killing the previous one if it's running
;; TODO: support on-exit callback
(define play!
  (with-mutex-lock *cplay-lock*
   (lambda (scommand #!optional (on-exit (lambda () (print "*** song finished"))))
     (print scommand)
     (if *cplay-proc*
         (condition-case (*cplay-proc* #:quit)
           (c (exn process)
              (ignore-wait-for-child-process-failed c))))

     (set! *cplay-proc* (process-cli (car scommand)
                                     (cdr scommand)
                                     on-exit))
     *cplay-proc*)))

(define (player-operation op)
  (with-mutex-lock
   *cplay-lock* (lambda () (and *cplay-proc* (*cplay-proc* op)))))

;; Control operations
(define player-pause
 (player-operation #:pause))

(define player-unpause
 (player-operation #:unpause))

(define player-quit
  (player-operation #:quit))

(define (play-command turi)
  ;; uri may be #f if uri-ref can't parse turi
  ;; TODO: if schema is tr, make rest request and find suri.
  ;; otherwise, use url directly
  (error "deprecated"))

(test-group
 "play-command"

 (test '("cplay" "file:///filename") (play-command "file:///filename"))
 (test '("cplay" "http://domain/file.mp3") (play-command "http://domain/file.mp3"))

 (test '("cplay" "filename") (play-command "filename"))
 (test-error (play-command "i l l e g a l")))
)

;; (define player (play! (play-command "tr://tone/440")))
;; (player #:quit)
