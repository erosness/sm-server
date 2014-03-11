(use test)

(test-group
 "udp-messages"
 (parameterize ((current-request (make-request)))
   (test
    "short body creates NOTIFY"
    "NOTIFY"
    (car (string-split
          (make-udp-message "/foo/bar" "some short body"))))
   (test
    "long body creates ALERT"
    "ALERT"
    (car (string-split
          (make-udp-message "/foo/bar" (make-string 1000 #\x)))))
   (test
    "length of message includes headers"
    "ALERT"
    (parameterize ((current-request
                    (make-request
                     headers: (headers `((echo ,(make-string 1000 #\x)))))))
      (car (string-split
            (make-udp-message "/foo/bar" "short body")))))))
