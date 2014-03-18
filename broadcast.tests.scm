(use test)

(test-group
 "udp-broadcast"
 (test (void) (udp-broadcast "hi"))
 (test-error (udp-broadcast (make-string 2048 #\a))))

(test-group
 "change-message"

 (test
  "ordinary NOTIFY message format"
  "NOTIFY /path\n\nbody"
  (change-message "/path" "body"))

 (test
  "(current-request)'s echo value is included"
  "NOTIFY /path\nEcho: ping\n\nbody"
  (parameterize ((current-request
                  (make-request
                   headers: (headers `((echo "ping"))))))
    (change-message "/path" "body")))

 ;; just to be clear, we're not doing this properly!
 (test "NOTIFY path with spaces\n\nbody"
       (change-message "path with spaces" "body")))

