(use test)

(test-group
 "change-message"


 ;; OBS: these tests assume json will be serialized in same order. is
 ;; this the case?
 (test
  "ordinary NOTIFY message format"
  (conc "NOTIFY {\"variable\":\"/path\","
        "\"owner\":{\"port\":false},"
        "\"data\":{\"body\":1},"
        "\"echo\":false}")
  (change-message "/path" `((body . 1)) #f))

 (test
  "(current-request)'s echo value is included"
  (conc "NOTIFY {\"variable\":\"/path\","
        "\"owner\":{\"port\":false},"
        "\"data\":101,"
        "\"echo\":\"ping\"}")
  (parameterize ((current-request
                  (make-request
                   headers: (headers `((echo "ping"))))))
    (change-message "/path" 101 #f)))

 (test "NOTIFY with port number"
       (conc "NOTIFY {\"variable\":\"path\","
             "\"owner\":{\"port\":10},"
             "\"data\":123,"
             "\"echo\":false}")
       (change-message "path" 123 10 )))
