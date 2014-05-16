;;; the most minimalistic search provider and turi converter I could write

(import turi rest)
(define-turi-adapter random-id->turi "random" (lambda (id) `((test . ,id))))
;; and you can use random-id->turi to generate these t2s-urls. try it:
;; (random-id->turi 4) ;; <-- should give you a tr:// url.

;; replace that tr:// url with http:// and give it a curl:
;;
;; curl "http://127.0.0.1/t2s?type=random&id=4"
;;
;; this will invoke the lambda above (with id as its argument).


;; let's make a search provider which constructs turis:
(define random-search
  (argumentize
   (lambda (i)
     (make-search-result
      0 10 10
      (list-tabulate 10 random-id->turi)))
   'q))

(define-handler /catalog/random (lambda () (random-search)) )

(use test)
(test
 ;; start the server for the port number to take affect
 "tr://localhost:5060/t2s?type=random&id=0"
 (->> (with-request "?q=blah" (/catalog/random))
      (alist-ref 'items)
      ((flip vector-ref) 0)))

