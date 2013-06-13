(use test)

;; an example uri-tree might be:
;; `((dir/ (file1 (.json ,"[1,3]")
;;                (.xml  ,"<ol><li>1</li><li>3</li></ol>"))))

(test-group
 "uri-handler"
 (define uris
   (uri-tree->alist
    `((1 (1 (1 #t)
            (2 #t))
         (2 (1 #t)
            (2 #t)))
      (2 (1 (1 (1 #t)
               (2 #t))
            (2 #t))))))

 ;; query each node and check it's #t
 (test (make-list 7 #t) (map (lambda (n) (alist-ref n uris equal?))
       '("111" "112"
         "121" "122"
         "2111" "2112"
         "212")))

 (test #f (alist-ref "1" paths equal? )))

