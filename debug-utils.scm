(define-syntax module*
  (syntax-rules ()
    ((module* name export body ...)
     (begin (module name * body ...)
            (import name)))))

(define-syntax comment
  (syntax-rules ()
    ((comment body ...)
     (void))))


(define-syntax module**
  (syntax-rules ()
    ((module** name export body ... )
     (begin body ...))))
