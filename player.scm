(use fmt test uri-common)

(include "player-util.scm")

;; create shell string for launching `cplay` player daemon. launch it
;; with play!.
(define (cplay uri #!optional seek)
  (conc "cplay "
        "\""
        (uri->string uri)
        "\""))

(test-group
 "cplay"
 (test "cplay \"filename\""
       (cplay (uri-reference "filename"))))

;; Spawn a subprocess. Use its line-based cli on stdin/stdout as
;; messaging interface. Returns a thread-safe cli procedure.
(define (launch-cplay command)
  (receive (pip pop pid)
      ;; spawn process:
      (process command)

    (define (cmd* . strings)
      (drain-input-port pip)
      (display (apply conc (intersperse strings " ")) pop)
      (display "\n" pop)
      (read-line pip))

    ;; thread safety:
    (define mx (make-mutex #|mutex label:|# `(play! ,command)))
    (define cmd (with-mutex-lock mx cmd*))

    ;; (define cmd cmd*) <-- back to non-threadsafe if you want to test

    (lambda (command . args)
      (case command
        ((#:stdout) pop)
        ((#:stdin)  pip)
        ((#:quit)  (process-signal pid))
        (else (apply cmd (cons command args)))))))


(define *cplay-lock* (make-mutex))
(define *cplay-proc* #f)

;; spawn command, killing the previous one if it's running
(define play!
  (with-mutex-lock
   *cplay-lock*
   (lambda (scommand)
     (print scommand)
     (if *cplay-proc* (*cplay-proc* #:quit))
     (set! *cplay-proc* (launch-cplay scommand))
     *cplay-proc*)))

(define (player-operation op)
  (with-mutex-lock
   *cplay-lock* (lambda () (and *cplay-proc* (*cplay-proc* op)))))

;; Control operations
(define (player-pause)
 (player-operation #:pause))

(define (player-unpause)
 (player-operation #:unpause))

(define (player-quit)
  (player-operation #:quit))


;; provide an API for audio hosts / providers to plug into.
(define *audio-hosts* `())
(define (define-audio-host host handler)
  (set! *audio-hosts*
        (alist-update host handler *audio-hosts* equal?)))

(define (play-command turi)
  ;; uri may be #f if uri-ref can't parse turi
  (let ((uri (if (uri? turi) turi (uri-reference turi))))
    (case (and uri (uri-scheme uri))
      ((tr) ((or
              ;; pick the procedure registered for host:
              (alist-ref (uri-host uri) *audio-hosts* equal?)
              ;; error if none found:
              (lambda _ (error "unknown audio host" (uri-host uri))))
             ;; call audio-host procedure with one arg:
             uri))
      ;; default to cplay with any other scheme (file://, http:// etc)
      (else (cplay (or uri (error "illegal uri" turi)))))))

(test-group
 "play-command"

 (test "cplay \"file:///filename\"" (play-command "file:///filename"))
 (test "cplay \"http://domain/file.mp3\"" (play-command "http://domain/file.mp3"))

 (test "cplay \"filename\"" (play-command "filename"))
 (test-error (play-command "i l l e g a l")))

;; (define stop (play! (cplay (uri-reference "tone://sine/440"))))
;; (read-string #f stop)
;; (stop)

(define player)
(when #f
  (define player
    (play! (cplay (uri-reference "file:///home/klm/cube/aosp-new/external/cplay/tones.m4a"))))

  (player "seek" 100)
  (player "pause")
  (player "pos")
  (player "unpause")
  (player #:quit))
