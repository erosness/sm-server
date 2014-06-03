;;;; stty.scm -- stty-like interface to termios
;;
;; Copyright (c) 2007-2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;; High-level interface
;;
;; Procedure: (stty [port] settings ...)
;;
;;   Sets the terminal attributes for PORT (defaulting to
;;   current-input-port) according to the SETTINGS, which should be a
;;   list of symbols corresponding to modes in the stty(1) man page,
;;   or one or more symbols wrapped in a (not ...) list.
;;
;;   To enable a character setting, use a list of the setting name
;;   followed by the character (or #f to disable), as in
;;
;;      (stty (erase #\delete))
;;
;;   The following settings are supported:
;;
;;      clocal cread crtscts cs5 cs6 cs7 cs8 cstopb hup hupcl parenb
;;      parodd brkint icrnl ignbrk igncr ignpar imaxbel inpck istrip
;;      ixany ixoff ixon parmrk tandem ocrnl onlcr onlret onocr opost
;;      tab0 tab1 tab2 tab3 tabs crterase crtkill ctlecho echo echoctl
;;      echoe echoke echonl echoprt icanon iexten isig noflsh prterase
;;      tostop xcase eof eol eol2 erase intr kill lnext quit rprnt
;;      start stop susp werase raw sane

;; Procedure: (with-stty '(setting ...) thunk)
;;
;;   Sets the terminal attributes with STTY, evaluates THUNK, then
;;   restores the original attributes and returns the value from
;;   THUNK.
;;
;;   Example:
;;
;;   (define (read-password prompt)
;;     (display prompt)
;;     (with-stty '(not echo) read-line))

;;;;; Low-level interface
;;
;; You shouldn't need to use this.
;;
;; Procedure: (get-terminal-attributes [port-or-fd])
;; Procedure: (set-terminal-attributes! port-or-fd action attrs)
;;
;; Procedure: (make-term-attrs)
;; Procedure: (free-term-attrs attrs)
;; Procedure: (term-attrs-iflag attrs)
;; Procedure: (term-attrs-oflag attrs)
;; Procedure: (term-attrs-cflag attrs)
;; Procedure: (term-attrs-lflag attrs)
;; Procedure: (term-attrs-cc attrs i)
;; Procedure: (term-attrs-iflag-set! attrs int)
;; Procedure: (term-attrs-oflag-set! attrs int)
;; Procedure: (term-attrs-cflag-set! attrs int)
;; Procedure: (term-attrs-lflag-set! attrs int)
;; Procedure: (term-attrs-cc-set! attrs i char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-library srfi-69 foreigners posix)

(module stty
 (stty with-stty
  get-terminal-attributes set-terminal-attributes!
  make-term-attrs free-term-attrs
  term-attrs-iflag term-attrs-iflag-set!
  term-attrs-oflag term-attrs-oflag-set!
  term-attrs-cflag term-attrs-cflag-set!
  term-attrs-lflag term-attrs-lflag-set!
  term-attrs-cc term-attrs-cc-set!
  term-attrs-ispeed term-attrs-ispeed-set!
  term-attrs-ospeed term-attrs-ospeed-set!
  TCSANOW TCSADRAIN TCSAFLUSH)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import scheme chicken extras posix srfi-69 foreign foreigners)

(declare (foreign-declare "#include <termios.h>\n"))
(declare (foreign-declare "typedef struct termios struct_termios;\n"))

(cond-expand
 ((and _HAVE_STRUCT_TERMIOS_C_ISPEED _HAVE_STRUCT_TERMIOS_C_OSPEED)
  (define-foreign-record-type (term-attrs struct_termios)
    (constructor: make-term-attrs)
    (destructor: free-term-attrs)
    (unsigned-long c_iflag term-attrs-iflag term-attrs-iflag-set!)
    (unsigned-long c_oflag term-attrs-oflag term-attrs-oflag-set!)
    (unsigned-long c_cflag term-attrs-cflag term-attrs-cflag-set!)
    (unsigned-long c_lflag term-attrs-lflag term-attrs-lflag-set!)
    (unsigned-char (c_cc 22) term-attrs-cc term-attrs-cc-set!)
    (unsigned-long c_ispeed term-attrs-ispeed term-attrs-ispeed-set!)
    (unsigned-long c_ospeed term-attrs-ospeed term-attrs-ospeed-set!)
    )
  )
 (else
  (define-foreign-record-type (term-attrs struct_termios)
    (constructor: make-term-attrs)
    (destructor: free-term-attrs)
    (unsigned-long c_iflag term-attrs-iflag term-attrs-iflag-set!)
    (unsigned-long c_oflag term-attrs-oflag term-attrs-oflag-set!)
    (unsigned-long c_cflag term-attrs-cflag term-attrs-cflag-set!)
    (unsigned-long c_lflag term-attrs-lflag term-attrs-lflag-set!)
    (unsigned-char (c_cc 22) term-attrs-cc term-attrs-cc-set!))
  (define term-attrs-ispeed      (lambda (_)  #f))
  (define term-attrs-ispeed-set! (lambda (_ _) #f))
  (define term-attrs-ospeed      (lambda (_)  #f))
  (define term-attrs-ospeed-set! (lambda (_ _) #f))
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(define-foreign-variable TCSANOW_ int "TCSANOW")
(define-foreign-variable TCSADRAIN_ int "TCSADRAIN")
(define-foreign-variable TCSAFLUSH_ int "TCSAFLUSH")
;; (define-foreign-variable TCSASOFT_ int "TCSASOFT")

(define TCSANOW TCSANOW_)
(define TCSADRAIN TCSADRAIN_)
(define TCSAFLUSH TCSAFLUSH_)
;; (define TCSASOFT TCSASOFT_)

(define-foreign-variable IGNBRK unsigned-long)
(define-foreign-variable BRKINT unsigned-long)
(define-foreign-variable IGNPAR unsigned-long)
(define-foreign-variable PARMRK unsigned-long)
(define-foreign-variable INPCK unsigned-long)
(define-foreign-variable ISTRIP unsigned-long)
(define-foreign-variable INLCR unsigned-long)
(define-foreign-variable IGNCR unsigned-long)
(define-foreign-variable ICRNL unsigned-long)
(define-foreign-variable IXON unsigned-long)
(define-foreign-variable IXOFF unsigned-long)
(define-foreign-variable IXANY unsigned-long)
(define-foreign-variable IMAXBEL unsigned-long)
;; (define-foreign-variable IUCLC unsigned-long)

(define-foreign-variable OPOST unsigned-long)
(define-foreign-variable ONLCR unsigned-long)
;; (define-foreign-variable OXTABS unsigned-long)
;; (define-foreign-variable ONOEOT unsigned-long)
(define-foreign-variable OCRNL unsigned-long)
;; (define-foreign-variable OLCUC unsigned-long)
(define-foreign-variable ONOCR unsigned-long)
(define-foreign-variable ONLRET unsigned-long)

(define-foreign-variable CSIZE unsigned-long)
(define-foreign-variable CS5 unsigned-long)
(define-foreign-variable CS6 unsigned-long)
(define-foreign-variable CS7 unsigned-long)
(define-foreign-variable CS8 unsigned-long)
(define-foreign-variable CSTOPB unsigned-long)
(define-foreign-variable CREAD unsigned-long)
(define-foreign-variable PARENB unsigned-long)
(define-foreign-variable PARODD unsigned-long)
(define-foreign-variable HUPCL unsigned-long)
(define-foreign-variable CLOCAL unsigned-long)
;; (define-foreign-variable CCTS_OFLOW unsigned-long)
(define-foreign-variable CRTSCTS unsigned-long)
;; (define-foreign-variable CRTS_IFLOW unsigned-long)
;; (define-foreign-variable MDMBUF unsigned-long)

(define-foreign-variable ECHOKE unsigned-long)
(define-foreign-variable ECHOE unsigned-long)
(define-foreign-variable ECHO unsigned-long)
(define-foreign-variable ECHONL unsigned-long)
(cond-expand
 (windows (define ECHOPRT 0))
 (else (define-foreign-variable ECHOPRT unsigned-long)))
(define-foreign-variable ECHOCTL unsigned-long)
(define-foreign-variable ISIG unsigned-long)
(define-foreign-variable ICANON unsigned-long)
;; (define-foreign-variable ALTWERASE unsigned-long)
(define-foreign-variable IEXTEN unsigned-long)
;; (define-foreign-variable EXTPROC unsigned-long)
(define-foreign-variable TOSTOP unsigned-long)
(define-foreign-variable FLUSHO unsigned-long)
;; (define-foreign-variable NOKERNINFO unsigned-long)
(define-foreign-variable PENDIN unsigned-long)
(define-foreign-variable NOFLSH unsigned-long)

(define-foreign-variable VEOF unsigned-long)
(define-foreign-variable VEOL unsigned-long)
(define-foreign-variable VEOL2 unsigned-long)
(define-foreign-variable VERASE unsigned-long)
;; (define-foreign-variable VERASE2 unsigned-long)
(define-foreign-variable VWERASE unsigned-long)
(define-foreign-variable VINTR unsigned-long)
(define-foreign-variable VKILL unsigned-long)
(define-foreign-variable VQUIT unsigned-long)
(define-foreign-variable VSUSP unsigned-long)
(define-foreign-variable VSTART unsigned-long)
(define-foreign-variable VSTOP unsigned-long)
;; (define-foreign-variable VDSUSP unsigned-long)
(define-foreign-variable VLNEXT unsigned-long)
(define-foreign-variable VREPRINT unsigned-long)
(define-foreign-variable VSTATUS unsigned-long)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic interface

(define get-term-attrs
  (foreign-lambda* int ((int fd) (c-pointer t))
    "return(tcgetattr(fd, (struct termios*) t));"))

(define (get-terminal-attributes port . o)
  (let* ((t (if (pair? o) (car o) (make-term-attrs)))
         (fd (if (port? port) (port->fileno port) port))
         (ok? (zero? (get-term-attrs fd t))))
    ;; free and return #f on failure
    (if (and (not ok?) (null? o))
        (free-term-attrs t))
    (and ok? t)))

(define set-term-attrs!
  (foreign-lambda* int ((int fd) (int action) (c-pointer t))
    "return(tcsetattr(fd, action, (struct termios*) t));"))

(define (set-terminal-attributes! port action t)
  (set-term-attrs! (if (port? port) (port->fileno port) port) action t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; symbolic representation of attributes

(define stty-lookup (make-hash-table eq?))

(for-each
 (lambda (c)
   (let ((type (cadr c))
         (value (caddr c)))
     (hash-table-set! stty-lookup (car c) (cdr c))))

 ;; ripped from the stty man page, then trimmed down to what seemed
 ;; available on most systems

 `(;; characters
;;    (dsusp    char     ,VDSUSP)   ; CHAR will send a terminal stop signal
   (eof      char     ,VEOF)     ; CHAR will send an EOF (terminate input)
   (eol      char     ,VEOL)     ; CHAR will end the line
   (eol2     char     ,VEOL2)    ; alternate CHAR for ending the line
   (erase    char     ,VERASE)   ; CHAR will erase the last character typed
   (intr     char     ,VINTR)    ; CHAR will send an interrupt signal
   (kill     char     ,VKILL)    ; CHAR will erase the current line
   (lnext    char     ,VLNEXT)   ; CHAR will enter the next character quoted
   (quit     char     ,VQUIT)    ; CHAR will send a quit signal
   (rprnt    char     ,VREPRINT) ; CHAR will redraw the current line
   (start    char     ,VSTART)   ; CHAR will restart output after stopping it
   (stop     char     ,VSTOP)    ; CHAR will stop the output
   (susp     char     ,VSUSP)    ; CHAR will send a terminal stop signal
   (werase   char     ,VWERASE)  ; CHAR will erase the last word typed

   ;; special settings
   (cols     special  #f) ; tell the kernel that the terminal has N columns
   (columns  special  #f) ; same as cols N
   (ispeed   special  #f) ; set the input speed to N
   (line     special  #f) ; use line discipline N
   (min      special  #f) ; with -icanon, set N characters minimum for a completed read
   (ospeed   special  #f) ; set the output speed to N
   (rows     special  #f) ; tell the kernel that the terminal has N rows
   (size     special  #f) ; print the number of rows and columns according to the kernel
   (speed    special  #f) ; print the terminal speed
   (time     special  #f) ; with -icanon, set read timeout of N tenths of a second

   ;; control settings
   (clocal   control  ,CLOCAL)  ; disable modem control signals
   (cread    control  ,CREAD)   ; allow input to be received
   (crtscts  control  ,CRTSCTS) ; enable RTS/CTS handshaking
   (cs5      control  ,CS5)     ; set character size to 5 bits
   (cs6      control  ,CS6)     ; set character size to 6 bits
   (cs7      control  ,CS7)     ; set character size to 7 bits
   (cs8      control  ,CS8)     ; set character size to 8 bits
   (cstopb   control  ,CSTOPB)  ; use two stop bits per character (one with `-')
   (hup      control  ,HUPCL)   ; send a hangup signal when the last process closes the tty
   (hupcl    control  ,HUPCL)   ; same as [-]hup
   (parenb   control  ,PARENB)  ; generate parity bit in output and expect parity bit in input
   (parodd   control  ,PARODD)  ; set odd parity (even with `-')

   ;; input settings
   (brkint   input    ,BRKINT)  ; breaks cause an interrupt signal
   (icrnl    input    ,ICRNL)   ; translate carriage return to newline
   (ignbrk   input    ,IGNBRK)  ; ignore break characters
   (igncr    input    ,IGNCR)   ; ignore carriage return
   (ignpar   input    ,IGNPAR)  ; ignore characters with parity errors
   (imaxbel  input    ,IMAXBEL) ; * beep and do not flush a full input buffer on a character
   (inlcr    input    ,INLCR)   ; translate newline to carriage return
   (inpck    input    ,INPCK)   ; enable input parity checking
   (istrip   input    ,ISTRIP)  ; clear high (8th) bit of input characters
;;    (iuclc    input    ,IUCLC)   ; * translate uppercase characters to lowercase
   (ixany    input    ,IXANY)   ; * let any character restart output, not only start character
   (ixoff    input    ,IXOFF)   ; enable sending of start/stop characters
   (ixon     input    ,IXON)    ; enable XON/XOFF flow control
   (parmrk   input    ,PARMRK)  ; mark parity errors (with a 255-0-character sequence)
   (tandem   input    ,IXOFF)   ; same as [-]ixoff

   ;; output settings
;;    (bs0      output   ,BS0) ; backspace delay style, N in [0..1]
;;    (bs1      output   ,BS1) ; backspace delay style, N in [0..1]
;;    (cr0      output   ,CR0) ; carriage return delay style, N in [0..3]
;;    (cr1      output   ,CR1) ; carriage return delay style, N in [0..3]
;;    (cr2      output   ,CR2) ; carriage return delay style, N in [0..3]
;;    (cr3      output   ,CR3) ; carriage return delay style, N in [0..3]
;;    (ff0      output   ,FF0) ; form feed delay style, N in [0..1]
;;    (ff1      output   ,FF1) ; form feed delay style, N in [0..1]
;;    (nl0      output   ,NL0) ; newline delay style, N in [0..1]
;;    (nl1      output   ,NL1) ; newline delay style, N in [0..1]
   (ocrnl    output   ,OCRNL) ; translate carriage return to newline
;;    (ofdel    output   ,OFDEL) ; use delete characters for fill instead of null characters
;;    (ofill    output   ,OFILL) ; use fill (padding) characters instead of timing for delays
;;    (olcuc    output   ,OLCUC) ; translate lowercase characters to uppercase
   (onlcr    output   ,ONLCR) ; translate newline to carriage return-newline
   (onlret   output   ,ONLRET) ; newline performs a carriage return
   (onocr    output   ,ONOCR) ; do not print carriage returns in the first column
   (opost    output   ,OPOST) ; postprocess output
   (tab0     output   #f) ; horizontal tab delay style, N in [0..3]
   (tab1     output   #f) ; horizontal tab delay style, N in [0..3]
   (tab2     output   #f) ; horizontal tab delay style, N in [0..3]
   (tab3     output   #f) ; horizontal tab delay style, N in [0..3]
   (tabs     output   #f) ; same as tab0
   ;;(-tabs    output   #f) ; same as tab3
;;    (vt0      output   ,VT0) ; vertical tab delay style, N in [0..1]
;;    (vt1      output   ,VT1) ; vertical tab delay style, N in [0..1]

   ;; local settings
   (crterase local    ,ECHOE)   ; echo erase characters as backspace-space-backspace
   (crtkill  local    ,ECHOKE)  ; kill all line by obeying the echoprt and echoe settings
   ;;(-crtkill local    #f) ; kill all line by obeying the echoctl and echok settings
   (ctlecho  local    ,ECHOCTL) ; echo control characters in hat notation (`^c')
   (echo     local    ,ECHO)    ; echo input characters
   (echoctl  local    ,ECHOCTL) ; same as [-]ctlecho
   (echoe    local    ,ECHOE)   ; same as [-]crterase
;;    (echok    local    ,ECHOK)   ; echo a newline after a kill character
   (echoke   local    ,ECHOKE)  ; same as [-]crtkill
   (echonl   local    ,ECHONL)  ; echo newline even if not echoing other characters
   (echoprt  local    ,ECHOPRT) ; echo erased characters backward, between `\' and '/'
   (icanon   local    ,ICANON)  ; enable erase, kill, werase, and rprnt special characters
;;   (iexten   local    ,IEXTEN)  ; enable non-POSIX special characters
   (isig     local    ,ISIG)    ; enable interrupt, quit, and suspend special characters
   (noflsh   local    ,NOFLSH)  ; disable flushing after interrupt and quit special characters
   (prterase local    ,ECHOPRT) ; same as [-]echoprt
   (tostop   local    ,TOSTOP)  ; stop background jobs that try to write to the terminal
;;    (xcase    local    ,XCASE)   ; with icanon, escape with `\' for uppercase characters

   ;; combination settings
   (LCASE    combine  (lcase))
   (cbreak   combine  (not icanon))
   (cooked   combine  (brkint ignpar istrip icrnl ixon opost isig icanon))
                                        ; also eof and eol characters
                                        ; to their default values
   (crt      combine  (echoe echoctl echoke))
   (dec      combine  (echoe echoctl echoke (not ixany)))
                                        ; also intr ^c erase 0177 kill ^u
   (decctlq  combine  (ixany))
   (ek       combine  ()) ; erase and kill characters to their default values
   (evenp    combine  (parenb (not parodd) cs7))
   ;;(-evenp combine  #f) ; same as -parenb cs8
   (lcase    combine  (xcase iuclc olcuc))
   (litout   combine  (cs8 (not parenb istrip opost)))
   ;;(-litout  combine  #f) ; same as parenb istrip opost cs7
   (nl       combine  (not icrnl onlcr))
   ;;(-nl      combine  #f) ; same as icrnl -inlcr -igncr onlcr -ocrnl -onlret
   (oddp     combine  (parenb parodd cs7))
   (parity   combine  (evenp)) ; same as [-]evenp
   (pass8    combine  (cs8 (not parenb istrip)))
   ;;(-pass8   combine  #f) ; same as parenb istrip cs7
   (raw      combine  (not ignbrk brkint ignpar parmrk
                           inpck istrip inlcr igncr icrnl))
   (ixon     combine  (ixoff ixany imaxbel opost isig icanon)) ;; xcase iuclc
   ;;(time     combine  #f) ; 0
   ;;(-raw     combine  #f) ; same as cooked
   (sane     combine  (cread brkint icrnl imaxbel opost onlcr
                       isig icanon ;; nl0 cr0 bs0 vt0 ff0 ; tab0
                       echo echoe echoctl echoke ;; iexten echok
                       (not ignbrk igncr ixoff ixany inlcr ;; iuclc
                            ocrnl onocr onlret ;; olcuc ofill ofdel
                            echonl noflsh tostop echoprt))) ;; xcase
                                        ; plus all special characters to
                                        ; their default values
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; high-level interface

(define (stty . args)

  (let* ((port (if (and (pair? args) (port? (car args)))
                   (car args)
                   (current-input-port)))
         (attr (get-terminal-attributes port))
         (iflag (term-attrs-iflag attr))
         (oflag (term-attrs-oflag attr))
         (cflag (term-attrs-cflag attr))
         (lflag (term-attrs-lflag attr)))

    ;; parse change requests
    (let lp ((lst (if (and (pair? args) (port? (car args))) (cdr args) args))
             (flag #t))
      (cond
       ((pair? lst)
        (let ((command (car lst)))
          (cond
           ((pair? command) ;; recurse on sub-expr
            (lp command flag)
            (lp (cdr lst) flag))
           ((eq? command 'not) ;; toggle current setting
            (lp (cdr lst) (not flag)))
           (else
            (let* ((x (hash-table-ref/default stty-lookup command #f))
                   (type (if (pair? x) (car x) #f)))
              (case type
                ((input)
                 (if flag
                     (set! iflag (bitwise-ior iflag (cadr x)))
                     (set! iflag (bitwise-and iflag (bitwise-not (cadr x)))))
                 (lp (cdr lst) flag))
                ((output)
                 (if flag
                     (set! oflag (bitwise-ior oflag (cadr x)))
                     (set! oflag (bitwise-and oflag (bitwise-not (cadr x)))))
                 (lp (cdr lst) flag))
                ((control)
                 (if flag
                     (set! cflag (bitwise-ior cflag (cadr x)))
                     (set! cflag (bitwise-and cflag (bitwise-not (cadr x)))))
                 (lp (cdr lst) flag))
                ((local)
                 (if flag
                     (set! lflag (bitwise-ior lflag (cadr x)))
                     (set! lflag (bitwise-and lflag (bitwise-not (cadr x)))))
                 (lp (cdr lst) flag))
                ((special)
                 (error "special settings not yet supported"))
                ((char)
                 (term-attrs-cc-set! attr (cadr x) (or (cadr lst) 0))
                 (lp (cddr lst) flag))
                ((combine) ;; recurse on def of this command
                 (lp (cadr x) flag)
                 (lp (cdr lst) flag))
                (else
                 (warning "unknown stty command" command)
                 (lp (cdr lst) flag))))))))))

    ;; set new values
    (term-attrs-iflag-set! attr iflag)
    (term-attrs-oflag-set! attr oflag)
    (term-attrs-cflag-set! attr cflag)
    (term-attrs-lflag-set! attr lflag)
    (set-terminal-attributes! port TCSANOW attr)
    (free-term-attrs attr)))

(define (with-stty setting thunk)
  (let* ((port (current-input-port))
         (orig-attrs (get-terminal-attributes port)))
    (dynamic-wind
        (lambda ()
          (stty setting))
        thunk
        (lambda ()
          (set-terminal-attributes! port TCSANOW orig-attrs)
          (free-term-attrs orig-attrs)))))

)
