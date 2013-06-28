(use posix clojurian-syntax srfi-1)

;; this tool doesn't work properly:
;; it doesn't find external dependencies' dependencies
;; e.g, if you depend on 'test' it will fetch it for you,
;; but it will miss test's regex dependency. you'll have to
;; do that manually for now. cd into (td) and `chicken-install -r` it.
;; this is good for setting up symlinks, at least

;; root directory to search for meta-files
(define wd (make-parameter (or (get-environment-variable "WD") "./")))
(define td (make-parameter (or (get-environment-variable "TD") ".module-sources")))
(define sys (make-parameter (lambda (str)
                              (fprintf (current-error-port) (conc "$$ " str "\n"))
                              (system str))))

;; recursively find all .meta files
;; unix rivalry:
;; (system "find -name \"*.meta\" ")
(define (metas)
  (let loop ((dirs (list (wd))))
    (if (pair? dirs)
        (append
         (glob (make-pathname (car dirs) "/*.meta"))
         (loop (append
                (filter directory?
                        (glob (make-pathname (car dirs) "/*")))
                (cdr dirs))))
        '())))

;; OBS! all modules must have a file <module-name>.meta
;; "dsp/biquad/biquad.meta" => (biquad . "dab/biquad/")
(define (locals)
  (map (lambda (path)
         (cons (-> path
                   (pathname-strip-extension)
                   (pathname-strip-directory)
                   (string->symbol))
               (pathname-directory path))) (metas)))

;; local modules's dependencies (unsorted union)
(define (deps)
  (->>
   (metas)
   (map (cut with-input-from-file <> read) ) ;; read/parse from disk
   (map (cut alist-ref 'depends <>)) ;; pick out 'depends' from alist
   (filter identity)                 ;; remove #f's
   (apply append)                    ;; make into a single list
   (map (lambda (d) (if (list? d) (car d) d))) ;; remove version information
   (delete-duplicates)))

(define (download!)
  (create-directory (td) #t) ;; #t for create-parents
  (->> (deps)
       (for-each (lambda (egg)
                   ((sys) (conc "cd " (td) " ; "
                                "chicken-install -r " egg))))))

(define (symlink!)
  (for-each
   (lambda (mod)
     (let ((name (car mod))
           (dir (cdr mod)))
       (let ((target-dir (make-pathname (td) (->string name))))
         ;; chicken-install can create empty dirs, delete them first
         ((sys) (conc "rmdir " target-dir))
         ;; wierd 'ln' behaviour: if symlink exists, its created in a
         ;; different!
         (if (file-exists? target-dir)
             (print "--- symlink already exists: " target-dir)
             ((sys) (conc "ln -s " (make-absolute-pathname (current-directory) dir) " "
                          target-dir))))))
   (locals)))

(define (help)
  (print #<<END
;; welcome to a very hacky way to install unpublished modules
;; hacking around this problem:
;; http://paste.call-cc.org/paste?id=aa1321c29285b12b0f2eb6cdcff95bdbeb798518#a1
;; we are doing two things:
       ;; downloading all dependency sources into $TD
       ;; symlinking all local modules into $TD
       ;; so that
       ;;      chicken-install -t local -l $TD
       ;; will work

(metas)  ;; list all meta files
(locals) ;; list local modules
(deps)   ;; list local modules' dependencies
(wd "../<new-directory>") ;; change metafile search-dir
(td "../<egg-sources>") ;; change target folder (must be outside of wd!)
(download!)          ;; download dependency sourcefiles (in ./)
(symlink!)          ;; create symlink for local modules (in ./)

(sys print)  ;; don't execute command, print them
(sys system) ;; back to execution
(sys (lambda (str)
       (print str)
       (system str))) ;; print and execute for total awesomeness

(help) ;; this help

;; or start with "run" on the command-line

END
)) 


(print "current directory is " (current-directory))
(print "target dir is " (td))
(print "meta-files: " )
(pp (metas))
(print "dependencies under " (wd) ":")
(pp (deps))
(print)



(if (member "run" (command-line-arguments))
    (begin (download!) (symlink!))
    (begin (help) (repl)))

(print "\n\nlocal module sources (symlinked)")
(system (conc "find " (td) " -type l"))

(print "\n\nempty source-directories: ")
(pp (filter (o null? directory) (glob "./.module-sources/*")))
