(include "playqueue.scm")

;; Adds an item to the back of the playqueue and starts it.
;; Returns: the passed in item with a unique id added
(define (/pq/play)
  (let* ((item (pq-add (current-json))))
    (pq-play (alist-ref 'id item))
    item))

;; Adds an item to the back of the playqueue
;; Returns: the passed in item with a unique id added
(define (/pq/add)
  (pq-add (current-json)))


;; Removes and item referenced by id from the playqueue
;; Does nothing if id is not found in playqueue
(define (/pq/del)
  (let* ((id (alist-ref 'id (current-json))))
    (pq-del id)
    `((status . "ok"))))

;; Removes every item from the playqueue and stops the player
(define (/pq/clear)
  (pq-clear)
  (player-quit)
  `((status . "ok")))

;; Returns the playqueue
(define (/pq) (list->vector *pq*))

;; PQ operations
(define-handler "/pq/play"  /pq/play)
(define-handler "/pq/add"   /pq/add)
(define-handler "/pq/clear" /pq/clear)
(define-handler "/pq/del"   /pq/del)
(define-handler "/pq" /pq)

