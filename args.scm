(use args)

;; cube-server and cube-speaker specific argument parsing

(define (usage opts)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "usage: " "label [port]")
      (print (args:usage opts))
      (print ""))))

;; call procs with (options operand1 operand2 ...)
(define (with-args opts proc)
  (receive (options operands)
      (args:parse (command-line-arguments)
                  (cons (args:make-option (h help) #:none "show usage" (usage opts) (exit)) opts))
    (if (>= (length operands) 1)
        (apply proc (cons options operands))
        (begin (usage opts) (exit)))))
