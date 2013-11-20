(module i2c *

(import chicken scheme foreign)
(use posix)

#>
#include <sys/ioctl.h>
<#

;; integer typed variant of ioctl (as opposed to the ioctl egg's
;; pointers)
(define ioctl-int
  (foreign-lambda* int ((int fd) (int command) (int arg))
              "return(ioctl(fd, command, arg));"))

(define (i2c-set-slave-addr fd i2c-addr)
  ;; taken from linux/i2c-dev.h (not present in android-build system)
  (define I2C_SLAVE #x0703)
  (ioctl-int fd I2C_SLAVE i2c-addr))

(define (i2c-open path #!optional (i2c-addr #x34))
  (define fd (file-open path open/rdwr))
  (i2c-set-slave-addr fd i2c-addr)
  fd)

(define i2c-close file-close)
)
