(use srfi-69) ;; hash-tables

;; allows writing nested path-rules
;; convert something like
;; `((dir1/ (file1 (.json data))) (file2 more-data)) =>
;; (("dir1/file1.json" . data) ("file2" . more-data))
(define (uri-tree->alist paths #!optional (initial "") (join-proc conc))
  (let loop ((prefix initial)
             (paths  paths))
    (append-map
     (lambda (paths)
       (if (pair? paths)
           (loop (join-proc prefix (car paths)) (cdr paths))
           (list (cons prefix paths))))
     paths)))

