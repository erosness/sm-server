(use feature-test)

#> #include <termios.h><#

(declaration-prefix SCM)
(registration-prefix "")

(define-foreign-features
  _HAVE_STRUCT_TERMIOS_C_ISPEED
  _HAVE_STRUCT_TERMIOS_C_OSPEED)
