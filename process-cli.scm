;;; process-cli: create a simple and synchronous interface to an async
;;; background-process, with an optional callback for when this
;;; process exits.
;;;
;;; - no process kill with signal/pipe if you (cli "command") after child exit
;;; - input port (pip) does not block other threads
;;; - fixes hang-bug when spawing multiple processes
;;; - disable buffering completely
;;; - better/simper thread-safety management
;;; - introduces on-exit callback (called from read-thread)

(module process-cli (process-cli)

(import chicken scheme data-structures)
(use posix extras ports srfi-13 srfi-18 posix srfi-1 clojurian-syntax)

;; don't exit on signal/pipe!
;; see http://krokisplace.blogspot.no/2010/02/suppressing-sigpipe-in-library.htlm
(set! (signal-handler signal/pipe) #f)

;; like open-input-file* but doesn't block other threads. obs: this
;; port isn't thread-safe (it may block all threads if used from
;; multiple threads). note that it's unbuffered.
(define (open-input-file*/nonblock fd)
  (make-input-port
   (lambda ()
      (thread-wait-for-i/o! fd #:input)
      (let ((r (file-read fd 1)))
        (if (= 1 (cadr r)) ;; number of bytes read must = 1
            (string-ref (car r) 0)
            #!eof)))
    (lambda () (file-select fd #f 0))
    (lambda () (file-close fd))))

;; like open-output-file* but doesn't buffer anything.
(define (open-output-file*/nobuffer fd)
   (make-output-port (lambda (x) (file-write fd x))
                     (lambda ()  (file-close fd))))

;;; process* fix from Moritz (http://bugs.call-cc.org/ticket/766).
;;; non-blocking, line-buffered cli from a subprocess.
(define (spawn* cmd #!optional args env)
  (let*-values
      (((in-in   in-out) (create-pipe))
       ((out-in out-out) (create-pipe))
       ((pid) (process-fork
               (lambda ()
                 (duplicate-fileno in-in fileno/stdin)
                 (duplicate-fileno out-out fileno/stdout)
                 (file-close in-out)
                 (file-close in-in)
                 (file-close out-in)
                 (file-close out-out)
                 (process-execute cmd args env)))))

    (file-close in-in)
    (file-close out-out)

    (values (open-input-file*/nonblock  out-in)
            (open-output-file*/nobuffer in-out)
            pid)))


;; Spawn a subprocess. Use its line-based cli on stdin/stdout as
;; messaging interface. Returns a thread-safe cli procedure. on-exit
;; will be called async when process finished (not with (cli #:quit)).
;;
;; OBS: processes need to be explicitly exit (see #:quit)
(define (process-cli command args on-exit)

  (assert (procedure? on-exit))

  (define mutex (make-mutex))
  (define read-mutex (make-mutex))
  (define last-line #f)
  (define cvar (make-condition-variable))

  (receive (pip pop pid)
      ;; spawn process:
      (spawn* command args)

    (define (send-command args)
      (let ((command-string (apply conc (intersperse args " "))))
        (if (string-any (lambda (char) (eq? #\newline char)) command-string)
            (error "command cannot contain newlines" command-string))
        (display command-string pop)
        (display #\newline pop)
        (flush-output pop)))

    ;; we spawn a separate read-thread and synchronize with the
    ;; command. we need a separate thread in case the process exits
    ;; asynchronously and we have to call on-exit.
    (define read-thread
      (make-thread (lambda ()
                     (let loop ()
                       (set! last-line (read-line pip))
                       (condition-variable-signal! cvar)
                       (if (not (eof-object? last-line))
                           (loop)))

                     (close-input-port pip)
                     (close-output-port pop)
                     (print ";; waiting for " pid)
                     ;; wait and detach from child
                     (process-wait pid)
                     (and on-exit (on-exit))
                     (print ";; read-thread " (current-thread) " done )"))
                   (conc "(ms" (current-milliseconds) ")")))

    (thread-specific-set! read-thread #t)
    (thread-start! read-thread)

    (define (cmd . strings)
      ;; gain exclusive rights for output-port
      (dynamic-wind
        (lambda () (mutex-lock! mutex))
        ;; do this with exclusive rights:
        (lambda ()
          (set! last-line #f)
          (send-command strings)
          (mutex-unlock! read-mutex cvar 1.0) ;; <-- emergency timeout
          (if last-line last-line
              (error "read-thread died or process hangs")))
        ;; wait for signal by read-thread (unlock even on error)
        (lambda () (mutex-unlock! mutex #f 1.0))) ;;<-- emergency timeout
      )

    (lambda (command . args)
      (case command
        ((#:stdout) pop)
        ((#:stdin)  pip)
        ((#:pid)    pid)
        ((#:on-exit) (if (pair? args) (set! on-exit (car args))) on-exit)
        ((#:quit) (error "deprecated, don't use #:quit. it messes up everything."))
        (else (apply cmd (cons command args)))))))

;;(include "process-cli.tests.scm")

)
