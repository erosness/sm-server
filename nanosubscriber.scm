;; nanoif test client.
(use posix matchable test string-utils)
(import nanoif)

(define (parse-response resp)
  (print "Response from gstplay: " resp))

(define request (make-nano-if "ipc:///data/nanomessage/test.pub"))
