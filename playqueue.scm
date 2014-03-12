(module playqueue (pq-list
                   pq-prev
                   pq-play-prev
                   pq-next
                   pq-play-next
                   pq-clear
                   pq-del
                   pq-ref
                   pq-add
                   pq-play
                   make-pq)

(import chicken scheme data-structures srfi-1)
(use srfi-18 uri-common test uuid)

(import concurrent-utils player rest)

;; (include "concurrent-utils.scm")

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


;; ==================== pq record ====================
(define-record-type pq (%make-pq list mutex current)
  pq?
  (list pq-list pq-list-set!)
  (mutex pq-mutex)
  (current pq-current pq-current-set!))

(define-record-printer (pq x port)
  (display (conc "#<pq " (length (pq-list x)) " current: " (pq-current x) ">") port))

(define (make-pq #!optional (lst '()) (current #f))
  (%make-pq lst (make-mutex) current))

;; ****************************
;; public api

;; The id of the track currently playing, an item with id *pq-current*
;; should always be available in the playqueue
;; (define *pq-current* #f)


(define (pq-ref* pq item)
  (let ((id (alist-ref 'id item)))
    (find (lambda (elem) (equal? (alist-ref 'id elem) id))
          (pq-list pq))))

(define (pq-add* pq item)
  (assert-pq-item item)
  (and (alist-ref 'id item)
       (error "item already has id field" item))

  (let ((item (alist-cons 'id (uuid-v4) item)))
    (pq-list-set! pq (add-back (pq-list pq) item))
    item))

(define (pq-del* pq item)
  (or (pq-ref* pq item) (error "cannot find" item))
  (pq-list-set! pq (delete item (pq-list pq))))

(define (pq-clear* pq)
  (pq-list-set! pq '())
  (pq-current-set! pq #f))

(define (pq-next/lst* pq lst)
  (and
   (pq-current pq)
   (let* ((id (alist-ref 'id (pq-current pq)))
          (tail (find-tail
                 (lambda (x) (equal? id (alist-ref 'id x)))
                 lst)))
     ;; return the next element of the first match
     (and (pair? (cdr tail)) (cadr tail)))))

(define (pq-next* pq) (pq-next/lst* pq (pq-list pq)))
(define (pq-prev* pq) (pq-next/lst* pq (reverse (pq-list pq))))

(define (pq-play* pq item)
  (let* ((item (or (pq-ref* pq item) (error "not found in pq" item)))
         (track (alist-ref 'turi item)))
    (play! (play-command track))
    (print "playing " track)
    (pq-current-set! pq item)))

(define (pq-play-next* pq) (pq-play* pq (pq-next* pq)))
(define (pq-play-prev* pq) (pq-play* pq (pq-prev* pq)))

;; ==================== thread-safety ====================
(define (with-pq-mutex proc)
  ;; this ain't pretty
  (lambda (pq . args)
    ((with-mutex-lock (pq-mutex pq) (lambda () (apply proc (cons pq args)))))))

(begin
  (define pq-ref   (with-pq-mutex pq-ref*))
  (define pq-add   (with-pq-mutex pq-add*))
  (define pq-del   (with-pq-mutex pq-del*))
  (define pq-clear (with-pq-mutex pq-clear*))
  (define pq-next  (with-pq-mutex pq-next*))
  (define pq-prev  (with-pq-mutex pq-prev*))

  (define pq-play  (with-pq-mutex pq-play*))
  (define pq-play-next  (with-pq-mutex pq-play-next*))
  (define pq-play-prev  (with-pq-mutex pq-play-prev*)))


(test-group
 "playqueue"

 (let ((pq (make-pq)))
   (test "initially empty" '() (pq-list pq))
   (let ((new-item (pq-add* pq `((turi . "a")))))
     (test "1 item added" (list new-item) (pq-list pq))
     (pq-del* pq new-item))

   (let ((i1 (pq-add* pq `((turi . "1"))))
         (i2 (pq-add* pq `((turi . "2")))))
     (test "2 items: add" (list i1 i2) (pq-list pq))
     (pq-del* pq i1)
     (test "1 items: delete" (list i2) (pq-list pq)))

   (pq-add* pq `((turi . "x")))
   (pq-add* pq `((turi . "y")))
   (pq-clear* pq)
   (test "pq-clear" '() (pq-list pq))

   (test-error (pq-add* pq `((id . "hello") (turi . "a"))))
   (test-error (pq-del* pq `((id . "no existe")))))

 (test "no pq-current" #f (pq-next* (make-pq)))
 (test "no pq-current" #f (pq-prev* (make-pq)))

 (let ((pq (make-pq `( ((id . "a"))
                       ((id . "b"))
                       ((id . "c")) )
                    ;; current:
                    `((id . "b")))))
   (test "pq-next*" `((id . "c")) (pq-next* pq))
   (test "pq-prev*" `((id . "a")) (pq-prev* pq)))

 (let ((pq (make-pq `(((id . "a"))) '((id . "a")))))
   (test "past last is #f"
         #f (pq-next* pq))
   (test "before first is #f"
         #f (pq-prev* pq)))

 (test-error (pq-play* (make-pq) `((id . "a")))))
)
