;;; modify default test-handler to print summary instead of just
;;; FAIL/PASS. this makes it easier to spot if something went wrong
;;; with lots of tests without having to halt distract the flow.
;;;
;;; Usage: (use pefat) and your (test 1 1) will print differently.
;;;
;;; PEFAT: Peders Egg, Favorite Acronym and Testing
;;;
;;; for each test prints something like 1/2/32
;;; which means test-group-errors/failing-tests/total-tests-run

(module pefat *
(import chicken scheme)

(use test data-structures)

;; private helpers
(define %test-tests (make-parameter 0))
(define test-group-errors (make-parameter 0))

;; defonce
(define %old-test-handler
  (or (condition-case %old-test-handler ((exn runtime) #f))
      (current-test-handler)))

(define (green x) (string-append "\x1B[32m" (->string x) "\x1B[0m"))
(define (red x)   (string-append "\x1B[31m" (->string x) "\x1B[0m"))
(define (yellow x) (string-append "\x1B[33m" (->string x) "\x1B[0m"))

(define (pass)
  (let* ((fails (test-failure-count))
         (color (cond ((> fails 0) red)
                      ((> (test-group-errors) 0) yellow)
                      (else green))))
    (color (conc (test-group-errors) "/" fails "/" (%test-tests)))))

(current-test-handler
 (lambda (status expect x info)

   ;; this is very very silly. we need to put this here. without
   ;; proper counting information, default-test-applier will print the
   ;; header on every test-case (not just the first in the group).
   (cond ((current-test-group) =>
          (lambda (g)
            (test-group-inc! g 'count)
            (test-group-inc! g status))))

   (define (inc parameter)
     (parameter (+ 1 (parameter))))

   (inc %test-tests)

   (cond ((eq? status 'PASS) (print "[" (pass) "]"))
         (else (%old-test-handler status expect x info) ))))

;; check for errors outside of testcase
(current-test-group-reporter
 (lambda (group)
   (cond ((alist-ref 'ERROR (cdr group)) =>
          (lambda (num-errors) (let ((old-val (test-group-errors)))
                            (test-group-errors (+ old-val num-errors))))))))

(define (clear-test-errors!)
  (test-failure-count 0)
  (test-group-errors 0))
)
