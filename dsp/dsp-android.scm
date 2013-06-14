
#>
#include "sigma_impl.c"
<#

(define dsp-init        (foreign-lambda int "dsp_init"))
(define dsp-close       (foreign-lambda int "dsp_close"))
(define dsp-volume-set! (foreign-lambda int "dsp_vol" byte))
(define dsp-mute-set!   (foreign-lambda int "dsp_mute" bool))

(dsp-init)



