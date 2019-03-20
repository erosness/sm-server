#!/bin/csi

(use posix srfi-18 nanomsg matchable test string-utils clojurian-syntax looper)

(define (make-nano-req-server-socket addr)
  (let ((req-sock (nn-socket 'pair)))
    (nn-bind req-sock addr)
    req-sock))

(define (make-nano-push-server-socket addr)
  (let ((push-sock (nn-socket 'pub)))
    (nn-bind push-sock addr)
    push-sock))

(define req-sock (make-nano-req-server-socket "ipc:///data/nanomessage/test.pair"))
(define push-sock (make-nano-push-server-socket "ipc:///data/nanomessage/test.pub"))
(print "Got socket")

(define (nano-req)
    (let ((msg (nn-recv req-sock)))
      (print "In " (thread-name (current-thread)) " -> " msg)
;;      (nn-send req-sock (string-concatenate `("ok " msg)))))
      (nn-send req-sock  "ok tullball!" )))

(thread-start!
  (->>
    nano-req
;;    (loop/interval 0.01)
    (loop)
    ((flip make-thread) "NanoReqThread")))

(print "begin")

(define (make-str)
  (string-concatenate `("Dette er en test...."
    ,(number->string
      (time->seconds
        (current-time))))))

(thread-sleep! 10)
(nn-send push-sock (make-str))
(print "ping")
(thread-sleep! 10)
(nn-send push-sock (make-str))
(print "ping")
(thread-sleep! 10)
(nn-send push-sock (make-str))
(print "ping")
(thread-sleep! 10)
(nn-send push-sock (make-str))
(print "ping")
(thread-sleep! 10)
(nn-send push-sock (make-str))
(print "ping")
(thread-sleep! 10)
(nn-send push-sock (make-str))
(print "ping")
(thread-sleep! 10)
(nn-send push-sock (make-str))
(print "ping")
(thread-sleep! 10)
(nn-send push-sock (make-str))
(print "ping")



(thread-sleep! 1)
(nn-send push-sock str)
(print "end")
(exit)
