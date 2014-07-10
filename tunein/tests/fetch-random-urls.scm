;; traverse TuneIn in a random fashion and print raw Tune responses.
;; you can manyally add useful such responses to raw-urls.scm.

(use tunein test miscmacros)

(define (query path)
  (append-map tjson->cjsons (tunein-query path)))

(define (get-paths objs)
  (filter-map
   (lambda (x) (alist-ref 'uri x))
   objs))

(define (get-turis objs)
  (filter-map
   (lambda (x) (alist-ref 'turi x))
   objs))


(define turis '())
(define paths '("Browse.ashx"))

(use srfi-1)

;; return (values element-at-i lst-with-element-removed)
(define (extract lst i)
  (receive (head tail) (split-at lst i)
    (values (car tail)
            (append head (cdr tail)))))

(test-group
 "extract"
 (test '(a (b c)) (receive (extract '(a b c) 0)))
 (test '(b (a c)) (receive (extract '(a b c) 1)))
 (test '(c (a b)) (receive (extract '(a b c) 2))))

(define (path-step paths)

  ;; pick a random path and process it
  (let-values (((path paths)
                (extract paths (random (length paths)))))

    (define objs (query path))
    (define paths+ (get-paths objs))
    (define turis+ (get-turis objs))

    (print)
    (for-each (lambda (turi) (pp (query turi))) turis+)

    (append paths+ paths)))

;; loop forever. you will eventually get 404 access denied by TuneIn
;; because of too many requests (without a partner id you get about 100/day)
(let loop ((paths paths))
  (loop (path-step paths)))
