(use srfi-18 uri-common test uuid)

;; the playqueue is the abstractions clients use to manipulate whats
;; currently playing. the contents of a playqueue is a playqueue item
;; (pq-item); which is an alist with the key 'turi present. when an
;; item is inserted into the queue it gets an unique id assigned to it
;; under the key 'id.
;;
;; the playqueue supports the following operations
;;
;;  - add: adds an element to the back of the queue
;;  - del: remove and item by id from the queue
;;  - clear: remove all items from the queue
;;  - play: play an item referenced by id




(define (add-back l item)
  (append l (list item)))

(test-group
 "add-back"
 (test "add to empty list" '(1)     (add-back '()    1))
 (test "add to non-empty"  '(1 2 3) (add-back '(1 2) 3)))

(define (assert-pq-item item)
  (assert (alist-ref 'turi item)))

(test-group
 "assert-pq-item"
 (test-assert "valid item" (assert-pq-item '((turi . "tr://wimp/play"))))
 (test-error  "wrong key"  (assert-pq-item `((foo . "tr://wimp/play")))))

(define *pq-lock* (make-mutex))
(define (make-pq) '())

;; lookup functions
(define (find-with-id id)
  (find (lambda (elem) (equal? (alist-ref 'id elem) id))
        *pq*))

;; (define (find-next id)
;;   (and-let* ((item (find-with-id id)))
;;             (cadr (memq item *pq* ))))

;; (define (find-prev id)
;;   (and-let* ((item (find-with-id id)))
;;             (cadr (memq item (reverse *pq*)))))


;; ****************************
;; public api

(define *pq* (make-pq))

;; The id of the track currently playing, an item with id *pq-current*
;; should always be available in the playqueue
(define *pq-current* #f)

(define pq-add
  (with-mutex-lock
   *pq-lock* (lambda (item)
               (assert-pq-item item)
               (if (find-with-id (alist-ref 'id item))
                   item
                 (let ((item (alist-cons 'id (uuid-v4) item)))
                   (pp item)
                   (set! *pq* (add-back *pq* item))
                   item)))))

(define pq-del
  (with-mutex-lock
   *pq-lock* (lambda (id)
               (and-let* ((item (find-with-id id)))
                         (set! *pq* (delete item *pq*))))))

(define pq-clear
  (with-mutex-lock
   *pq-lock* (lambda ()
               (set! *pq* (make-pq))
               (set! *pq-current* #f))))

(begin
  (pq-clear)
  (test-assert
   "play queue empty"
   (equal? *pq* (make-pq)))
  (pq-add '((turi . "hoho")))
  (pq-add '((turi . "haha")))
  (test-error
   "will fail since key is wrong" 
   (pq-add '(wont-be . "added")))
  (pq-del (alist-ref 'id (car *pq*)))
  (test-assert
   "contains one element"
   (equal? (length *pq*) 1))
  (pq-clear)
  (test-assert
   "play queue is now empty"
   (equal? *pq* (make-pq))))


;; (define pq-next
;;   (with-mutex-lock
;;    *pq-lock* (lambda ()
;;                (let ((item (find-next *pq-current*)))
;;                  (pq-play (alist-ref 'id item))))))

;; (define pq-prev
;;   (with-mutex-lock
;;    *pq-lock* (lambda ()
;;                (let ((item (find-prev *pq-current*)))
;;                  (pq-play (alist-ref 'id item))))))


(define (pq-play uuid)
  (with-mutex-lock
   *pq-lock*
   (and-let* ((item (find-with-id uuid))
              (track (alist-ref 'turi item)))
             (play! (play-command track))
             (print "playing " track)
             (set! *pq-current* uuid))))
