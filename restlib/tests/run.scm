(use restlib test spiffy intarweb uri-common)


(test-group
 "current-query-param"
 (parameterize ((current-request (make-request uri: (uri-reference "com?a=x&b=y&a+b=x+y"))))
   (test "x" (current-query-param 'a))
   (test "y" (current-query-param 'b))
   (test "x y" (current-query-param '|a b|))))


(test-group
 "maybe-drop"
 (test '(a) (maybe-drop #f '(a)))
 (test '(a) (maybe-drop 0 '(a)))
 (test '()  (maybe-drop 1 '(a)))
 (test '()  (maybe-drop 2 '(a))))

(test-group
 "maybe-take"
 (test '(a) (maybe-take #f '(a)))
 (test '()  (maybe-take 0 '(a)))
 (test '(a) (maybe-take 1 '(a)))
 (test '(a) (maybe-take 2 '(a))))

(test-group
 "paginate list"
 (test '((limit . 0) (offset . 0) (total . 3) (items . #()))    (paginate '(a b c) 0 0))
 (test '((limit . 1) (offset . 0) (total . 3) (items . #(a)))   (paginate '(a b c) 1 0))
 (test '((limit . 2) (offset . 0) (total . 3) (items . #(a b))) (paginate '(a b c) 2 0))
 (test '((limit . 1) (offset . 1) (total . 3) (items . #(b)))   (paginate '(a b c) 1 1))
 (test '((limit . 1) (offset . 2) (total . 3) (items . #(c)))   (paginate '(a b c) 1 2))
 (test '((limit . 1) (offset . 3) (total . 3) (items . #()))    (paginate '(a b c) 1 3)))

(test-group
 "querify"
 (test '(a aa ab ac ba ca) ((querify '(a b c aa ab ac ba bb bc ca cb cc)) "a")))

