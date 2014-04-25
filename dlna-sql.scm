(module dlna-sql (current-db browse search)

(import chicken scheme data-structures srfi-1)

(use (except sql-de-lite reset))

;;; dlna-sql -- browse and search databases generated by miniDLNA
(define current-db (make-parameter #f))

;; Statements
(define base-string (conc "SELECT o.OBJECT_ID, o.CLASS, o.NAME, d.DURATION, d.ARTIST, "
                          "d.ALBUM, d.PATH from OBJECTS o left join DETAILS d on (d.ID = o.DETAIL_ID) "))

(define browse-string (conc base-string "where PARENT_ID = ?"))
(define (browse-stmt db) (sql db browse-string))

;; nested SELECT keeps sqlite from concatenating the query string for
;; each row. see: http://sqlite.1065341.n5.nabble.com/LIKE-operator-with-prepared-statements-td8553.html
(define search-string (conc base-string "where OBJECT_ID glob '*$*' and (d.TITLE like (SELECT '%' || ? || '%'))"))
(define (search-stmt db) (sql db search-string))

;; Transforms
(define (row-> fields row)
  (map (lambda (x y) (cons x y)) fields row))

(define (row->track row)
  (row-> '(id type title duration artist album tid) row))
(define (row->folder row)
  (row-> '(id type name) row))

(define (process-rows rows)
  (filter identity
          (map
           (lambda (row)
             (cond ((equal? "container.storageFolder" (cadr row)) (row->folder row))
                   ((equal? "item.audioItem.musicTrack" (cadr row)) (row->track row))
                   (else #f))) rows)))


(define (wrap-db-error thunk)
  (condition-case (thunk)
    (e ()
       ;; TODO: can we do this without looking at exception message?
       (let ((msg ((condition-property-accessor 'exn 'message) e)))
         (if (equal? msg "unrecognized database type")
             (begin (print "db not found: " (current-db)) #f)
             (abort e))))))

(define (%do-query stmt arg)
  (wrap-db-error
   (lambda () (call-with-database
          (current-db) (lambda (db)
                 (process-rows (query fetch-all (stmt db) arg)))))))


;;; Public api
(define (browse #!optional (id 1))
  (%do-query browse-stmt id))

(define (search q)
  (%do-query search-stmt q))
)
