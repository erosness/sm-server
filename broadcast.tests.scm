(use test)

(test-group
 "change-message"

 (test
  "ordinary NOTIFY message format"
  "NOTIFY /path\r\n\r\nbody"
  (change-message "/path" "body" #f))

 (test
  "(current-request)'s echo value is included"
  "NOTIFY /path\nEcho: ping\r\n\r\nbody"
  (parameterize ((current-request
                  (make-request
                   headers: (headers `((echo "ping"))))))
    (change-message "/path" "body" #f)))

 ;; just to be clear, we're not doing this properly!
 (test "NOTIFY path with spaces\r\n\r\nbody"
       (change-message "path with spaces" "body" #f))

 (test "NOTIFY with port number"
       "NOTIFY path\nPort: 10\r\n\r\nbody"
       (change-message "path" "body" 10 )))
