

(define (version-information)
  (let-syntax
      ((cube-version
        (ir-macro-transformer
         (lambda (x e t)
           (string-trim-right
            (with-input-from-pipe
             "git describe --tags --always"
             read-string))))))

    `((cube-server . ,(cube-version))
      (cplay . ,(string-trim-right
               (with-input-from-pipe
                "cplay --version"
                read-string))))))

(define-handler /v1/version version-information)
