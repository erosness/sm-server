

(define (version-information)
  (let-syntax
      ((cube-version
        (ir-macro-transformer
         (lambda (x e t)
           (use posix)
           (string-trim-right
            (with-input-from-pipe
             ;; if git describe in pwd fails, desparately try the
             ;; hardcoded android-build-directory. note that this
             ;; shell-command is run at compile-time.
             (conc "git describe --tags --always || "
                   "(cd $ANDROID_BUILD_TOP/external/cube-server && git describe --tags --always)")
             read-string))))))

    `((cube-server . ,(cube-version))
      (cplay . ,(string-trim-right
               (with-input-from-pipe
                "cplay --version"
                read-string))))))

(define-handler /v1/version version-information)
