#!/bin/csi

(use posix nanomsg matchable test)

(define (parse-response resp)
  (print "Response from gstplay: " resp))

(define (parse-cplay-pos-response resp)
  (match (string-split resp)
    (("ok" pos duration)
     (values (string->number pos)
             (string->number duration)))
    (else (values 0 0))))

    (define (symbol-list->string cmd-list)
      (let* ((token-raw (car cmd-list))
            (token-str (if (symbol? token-raw)
           	                (symbol->string token-raw)
                            token-raw)))
              (if (not (null? (cdr cmd-list)))
                (string-append token-str " " (symbol-list->string (cdr cmd-list)))
                token-str)))

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
  (let* ((cmd-string* (symbol-list->string cmd))
          (cmd-string (string-append cmd-string* "\n")))
    (print "::: " cmd-string)
    (nn-send nnsock-gst cmd-string)
    (let ((response (nn-recv nnsock-gst)))
      (print "Response: " response)
      (if parser
        (parser response)))))


(define (play-worker msg)
  (print "MSG=" msg)
  (match msg
    (('pos) (gstplay-cli '(pos) parse-cplay-pos-response))
    (('duration)
      (call-with-values ;; better way to do this?
        (lambda () (gstplay-cli "pos\n" parse-cplay-pos-response))
        (lambda (pos #!optional duration) duration)))

    (('paused?) (gstplay-cli "paused?\n" parse-cplay-paused?-response))
    (('play pcommand on-exit) (gstplay-cli `(play ,pcommand)))
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

;;(define (player-nexttrack turi)
;;  (let ((nxt  (next-command turi)))
;;    (play-worker `(nexttrack ,nxt))))



(define (play! pcommand on-exit on-next)
  (prepause-spotify)
;;  (setup-nexttrack-callback on-next)
  (play-worker `(play ,pcommand on-exit)))

  (player-pos)
  (sleep 1)
  (player-pos)
  (sleep 1)
  (print ": " (nn-recv* nnsock-gst nn/dontwait))
  (sleep 1)
  (play! "http://listen.181fm.com/181-70s_128k.mp3?noPreRoll=true" (lambda () (print "At A")) (lambda () (print "At B")) )
;;  (nn-send nnsock-gst "play http://listen.181fm.com/181-70s_128k.mp3?noPreRoll=true\n")
  (sleep 1)
  (print ": " (nn-recv* nnsock-gst nn/dontwait))
  (sleep 1)
  (print ": " (nn-recv* nnsock-gst nn/dontwait))
  (sleep 1)
  (print ": " (nn-recv* nnsock-gst nn/dontwait))
  (sleep 1)
  (print ": " (nn-recv* nnsock-gst nn/dontwait))
  (sleep 1)
  (print ": " (nn-recv* nnsock-gst nn/dontwait))
  (sleep 1)
  (print ": " (nn-recv* nnsock-gst nn/dontwait))
  (sleep 1)

(exit)
