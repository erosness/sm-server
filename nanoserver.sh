#!/bin/csi

(use posix srfi-18 nanomsg matchable test string-utils clojurian-syntax looper medea)

(define (make-nano-req-server-socket addr)
  (let ((req-sock (nn-socket 'pair)))
    (nn-bind req-sock addr)
    req-sock))

(define (make-nano-push-server-socket addr)
  (let ((push-sock (nn-socket 'pub)))
    (nn-bind push-sock addr)
    push-sock))

(define req-sock (make-nano-req-server-socket "ipc:///data/nanomessage/bt.pair"))
(define push-sock (make-nano-push-server-socket "ipc:///data/nanomessage/bt.pub"))

(print "Begin")

(define (nano-req)
    (let ((msg (nn-recv req-sock)))
      (print "In " (thread-name (current-thread)) " -> " msg)
;;      (nn-send req-sock (string-concatenate `("ok " msg)))))
      (nn-send req-sock  "ok tullball!" )))

(define req-thread
  (thread-start!
    (->>
      nano-req
      (loop)
      ((flip make-thread) "NanoReqThread"))))

(print "Made thread:" req-thread " - " (thread-name req-thread))

(define (make-response-string)
  (string-concatenate `("Dette er en test...."
    ,(number->string
      (time->seconds
        (current-time))))))

(define msg-src
  (list
    `(( metadata ( title . "Travling Light")
      ( subtitle . "Minor Majority")
      ( paused . #f )))
    `(( metadata ( title . "This land")
      ( subtitle . "Blueberry")
      ( paused . #f )))
    `(( metadata ( title . "Horseshit")
      ( subtitle . "Abel")
      ( paused . #f )))
    `(( metadata ( title . "High'n dry")
      ( subtitle . "Hexås ørten ærø")
      ( paused . #f )))
  ))

(define (rotate lst)
 (let ((first (car lst))
       (rest (cdr lst)))
   (append rest (list first))))

(define (nano-push)
      (print "Sending push message")
      (nn-send push-sock  (json->string (car msg-src)))
      (set! msg-src (rotate msg-src))
      (nn-send push-sock  (json->string `(( status ( connection . 2 )
                                          ( player . "Erik")
                                          ( last_pair . 0 ))))))


(define push-thread
  (thread-start!
    (->>
      nano-push
      (loop/interval 5.0)
      (loop)
      ((flip make-thread) "NanoPushThread"))))

(print "Made thread:" push-thread " - " (thread-name push-thread))

(define (make-push-string)
  (string-concatenate
    `( "Dette er en test...."
      ,(number->string
        (time->seconds
          (current-time))))))

(thread-join! req-thread)
(thread-join! push-thread)

(exit)
