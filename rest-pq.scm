(module rest-pq ()

(import chicken scheme data-structures srfi-1)

(use restlib)

(import broadcast
        rest player
        playqueue)

(define *pq* (make-pq))

;; Adds an item to the back of the playqueue and starts it.
;; Returns: the passed in item with a unique id added
(define-handler /pq/play
  (bc (lambda ()
        (let* ((item (current-json))
               (existing (pq-ref *pq* item))
               (item (or existing (pq-add *pq* item))))
          (print "playing " item)
          (pq-play *pq* item)
          item))
      "/pq/play"))

;; Adds an item to the back of the playqueue
;; Returns: the passed in item with a unique id added
(define-handler /pq/add
  (bc (lambda () (pq-add *pq* (current-json))) "/pq/add"))


;; Removes and item referenced by id from the playqueue
;; Does nothing if id is not found in playqueue
(define-handler /pq/del
  (bc (lambda () (let* ((id (alist-ref 'id (current-json))))
              (pq-del *pq* id)
              `((status . "ok"))))
      "/pq/del"))


;; Removes every item from the playqueue and stops the player
(define-handler /pq/clear
  (bc (lambda () (pq-clear *pq*)
         (player-quit)
         `((status . "ok")))
      "/pq/clear"))

(define-handler /pq/play/next
  (bc (lambda () (let ((nx (pq-next *pq*)))
              (pq-play-next *pq*)
              nx))
      "/pq/play/next"))

(define-handler /pq/play/prev
  (bc (lambda () (let ((nx (pq-prev *pq*)))
              (pq-play-prev *pq*)
              nx))
      "/pq/play/prev"))

;; (/pq/play/next)
;; (/pq/play/prev)

;; Returns the playqueue
(define-handler /pq (lambda () (list->vector (pq-list *pq*))))
)
