
;;; Adds urls for browsing a pretend audio-source in our new nested
;;; json format.

(define-handler /dummy-tabs (lambda () `((tabs . #( ((title . "Tab 1") (uri . "/dummy/tab1"))
                                               ((title . "Tab 2") (uri . "/trick/tab2")))))))

(define-handler /dummy/tab1
  (lambda () (make-search-result 10 0 1
                            `( ((title . "My Folder")
                                (uri . "/dummy/tab1/folder1"))))))

(define-handler /dummy/tab1/folder1
  (lambda () (make-search-result 10 0 1
                            `( ((title . "Playable Track")
                                (turi . "tr://localhost/this-almost-works.mp3"))))))

(define-handler /trick/tab2
  (lambda () (make-search-result 10 0 333
                            `( ((title . "Folder 1")
                                (uri . "/dlna/browse/$1"))

                               ((title . "Folder 2")
                                (uri . "/dlna/browse/x"))

                               ((title . "Simply the Best")
                                (artist . "Tina Turner")
                                (image . "http://host/file.jpg")
                                (turi . "tr://10.0.0.1/dlna"))))))
