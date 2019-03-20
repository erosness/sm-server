
(use posix matchable test string-utils)
(import nanoif)

(define (parse-response resp)
  (print "Response from gstplay: " resp))

(define (nn-send* rec msg)
  (let ((sock (get-socket rec)))
    (nn-send sock msg)))

;; Add blocking thread to fetch meesages over nanomsg connection.
;; Currently only strict requst-respnse messages. TODO: out-of-band
;; push messages for tag update and status change (typically end-of-track)
(define (make-nano-pull-thread)
;; Read all messages in a blocking loop. Sort messages as response and
;; push messages based on grammar.
  (define (make-pull-socket)
     (let ((pull-sock (nn-socket 'sub)))
        (nn-connect pull-sock  "ipc:///data/nanomessage/test.pub")
        (nn-subscribe pull-sock "")
        pull-sock))

  (define (read-nanomsg)
    (let* ((pull-socket (make-pull-socket)))
;;      (nano-if-request rec `(pos))
      (print "Before... in " (thread-name (current-thread)))
      (let ((msg (nn-recv pull-socket)))
        (print "XX" pull-socket " - " msg))))

  (thread-start!
    (->>
      read-nanomsg
      (loop/interval 0.01)
      (loop)
      ((flip make-thread) "NanoReadThread"))))


(define (nano-if-request rec msg #!optional (parser #f))
  (let ((sock (get-socket rec))
        (mtx  (get-mutex rec) ))
    (dynamic-wind
      (lambda () (mutex-lock! mtx))
      (lambda ()
        (let* ((cmd-string* (symbol-list->string msg))
              (cmd-string (string-append cmd-string* "\n")))
          (nn-send* rec cmd-string)
          (let ((response (nn-recv sock)))
            (if parser
              (parser response)
              response))))
      (lambda () (mutex-unlock! mtx)))))

(define (get-msg rec)
  (let ((sock (get-socket rec)))
;;    (nn-recv* sock nn/dontwait)))
    (nn-recv sock)))

;; end general nano part
;; parsers

(define (parse-cplay-pos-response resp)
  (match (string-split resp " #\x0a;#\x00;")
    (("ok" pos duration)
      (let ((pos-number (string->number pos))
            (duration-number (string->number duration)))
        (values (if pos-number pos-number 0)
                (if duration-number duration-number 0))))
    (else (values 0 0))))

(test-group
 "parse cplay pos"
 (test "parse cplay pos - success"
       '(23.2341 42.43)
       (receive (parse-cplay-pos-response "ok 23.2341 42.43")))

  (test "parse cplay pos - success with tail"
       '(93.2341 45.23)
       (receive (parse-cplay-pos-response "ok 93.2341 45.23\n#\x00;")))

 (test "parse cplay pos - success with huge number and tail"
      '(1298129893.2341 45.23)
      (receive (parse-cplay-pos-response "ok 1298129893.2341 45.23\n#\x00;")))

 (test "parse cplay pos - success, report 0 for number garbage"
       '(0 45.23)
       (receive (parse-cplay-pos-response "ok per 45.23\n#\x00;")))

 (test "parse cplay pos - failure"
       '(0 0)
       (receive (parse-cplay-pos-response "some garbage 1234"))))


    (define (symbol-list->string cmd-list)
      (let* ((token-raw (car cmd-list))
            (token-str (if (symbol? token-raw)
           	                (symbol->string token-raw)
                            token-raw)))
              (if (not (null? (cdr cmd-list)))
                (string-append token-str " " (symbol-list->string (cdr cmd-list)))
                token-str)))

(define (parse-cplay-paused?-response resp)
 (and-let* ((value (string-split resp " #\x0a;#\x00;"))
            ((equal? (length value) 2))
            (value (cadr value))
            ((or (equal? value "false") (equal? value "true"))))
   (equal? value "true")))

(test-group
 "parse cplay paused?"
 (test "parse cplay pos - success, paused"
       '(#t)
       (receive (parse-cplay-paused?-response "ok true")))

 (test "parse cplay pos - success, not paused"
       '(#f)
       (receive (parse-cplay-paused?-response "ok false")))

 (test "parse cplay pos - fail, not paused"
       '(#f)
       (receive (parse-cplay-paused?-response "ok false per"))))


;; end parsers
;; player part


(define (prepause-spotify)
  (with-input-from-pipe "spotifyctl 7879 pause" void)
  (thread-sleep! 0.3))

;; Control operations
(define (player-pause)           (nano-if-request gstplayer `(pause)))
(define (player-unpause)         (prepause-spotify) (nano-if-request gstplayer `(unpause)))
(define (player-spotify-unpause) (nano-if-request gstplayer  `(unpause)))
(define (player-paused?)         (nano-if-request gstplayer `(paused?) parse-cplay-paused?-response))
(define (player-pos)             (nano-if-request gstplayer `(pos) parse-cplay-pos-response))
;;(define (player-pos)             (nano-if-request gstplayer `(pos)))
(define (player-duration)        (receive (pos duration)
                                   (nano-if-request gstplayer `(pos) parse-cplay-pos-response)
                                   duration))


(define (player-seek seek)       (prepause-spotify) (nano-if-request gstplayer `(seek ,seek)))
(define (player-quit)            (nano-if-request gstplayer  `(quit)))
;; cplay running and not paused:
(define (playing?)   (and (not (eq? #f (nano-if-request gstplayer `(pos))))
                          (not (player-paused?))))
(define (player-nexttrack?) (nano-if-request gstplayer  `(nexttrack?)))
(define (play! pcommand on-exit on-next)
;;  (setup-nexttrack-callback on-next)
  (prepause-spotify)
  (nano-if-request gstplayer pcommand))

(define gstplayer (make-nano-if "ipc:///data/nanomessage/test.pair"))
(define pull-thread (make-nano-pull-thread))

;; (define thread1 (make-nano-read-thread gstplayer))
;; Test part
;;(define gst-socket (make-nano "ipc:///data/nanomessage/playcmd.pair"))


(print (player-pos))
(thread-sleep! 1)
(print (player-pos))



(thread-sleep! 1)
;;(print ": " (get-msg gstplayer))
(thread-sleep! 1)
(print "Play:" (play! `("play" "http://listen.181fm.com/181-70s_128k.mp3?noPreRoll=true") (lambda () (print "At A")) (lambda () (print "At B")) ))

(print "Paused?: "(player-paused?))
(thread-sleep! 3)
(print "Paused?: "(player-paused?))
;;(print ": " (get-msg gstplayer))
(thread-sleep! 3)
(print (player-pos))
;;(print ": " (get-msg gstplayer))
(player-pause)
(thread-sleep! 3)
(print "Paused?: "(player-paused?))
(print "Duration?: "(player-duration))
(thread-sleep! 3)
;;(print ": " (get-msg gstplayer))
(print (player-pos))
(player-unpause)
(thread-sleep! 3)
(print "Paused?: "(player-paused?))
;;(print ": " (get-msg gstplayer))
(thread-sleep! 3)
;;(print ": " (get-msg gstplayer))
;;  (print (player-pos))
(thread-sleep! 3)
(print (player-pos))
;;(print ": " (get-msg gstplayer))
(thread-sleep! 3)
(print (player-pos))

(exit)
