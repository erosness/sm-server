;; nanoif test client.
(use posix matchable test string-utils nanoif)

(define (parse-response resp)
  (print "Response from gstplay: " resp))

;;(define request (make-nano-if
;;  "ipc:///data/nanomessage/test.pair"
;;  "ipc:///data/nanomessage/test.pub"))

(define request (make-nano-half-if
  "ipc:///data/nanomessage/test.pair"))

(define (handler msg)
  (print "Installed handler:" msg))


(nano-if-request request '("Heisann1") parse-response)
(thread-sleep! 0.7)
(nano-if-request request '("Heisann2") parse-response)
(thread-sleep! 0.4)
(nano-if-request request '("Heisann3") parse-response)
(thread-sleep! 1.1)
(nano-if-request request '("Heisann4") parse-response)
(thread-sleep! 11.2)
(nano-if-request request '("Heisann5") parse-response)
;;(set-handler request handler)
