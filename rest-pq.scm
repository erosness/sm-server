(include "playqueue.scm")

;; Adds an item to the back of the playqueue and starts it.
;; Returns: the passed in item with a unique id added
(define /pq/play
  (bc (lambda () (let* ((item (pq-add (current-json))))
              (pq-play (alist-ref 'id item))
              item))
      "/pq/play"))

;; Adds an item to the back of the playqueue
;; Returns: the passed in item with a unique id added
(define /pq/add
  (bc (lambda () (pq-add (current-json))) "/pq/add"))


;; Removes and item referenced by id from the playqueue
;; Does nothing if id is not found in playqueue
(define /pq/del
  (bc (lambda () (let* ((id (alist-ref 'id (current-json))))
              (pq-del id)
              `((status . "ok"))))
      "/pq/del"))


;; Removes every item from the playqueue and stops the player
(define /pq/clear
  (bc (lambda () (pq-clear)
         (player-quit)
         `((status . "ok")))
      "/pq/clear"))

;; Returns the playqueue
(define (/pq) (list->vector *pq*))

;; PQ operations
(define-handler "/pq/play"  /pq/play)
(define-handler "/pq/add"   /pq/add)
(define-handler "/pq/clear" /pq/clear)
(define-handler "/pq/del"   /pq/del)
(define-handler "/pq" /pq)
