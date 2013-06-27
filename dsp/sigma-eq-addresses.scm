#>
#define SIGMASTUDIOTYPE_FIXPOINT_CONVERT (x) 
#include "sigma-export/cube_130308_IC_1_PARAM.h"
<#

(let-syntax
    ;; foreign integer list
    ;; (fil A B) => (list (foreign-value "A" int) (foreign-value "B" int))
    ((fil 
      ;; no need to be hygienic :)
      (er-macro-transformer
       (lambda (x r t)
         `(list
           ,@(map
              (lambda (varname)
                `(foreign-value ,(symbol->string varname) int))
              (cdr x)))))))
  (list
   (fil MOD_MIDEQ1_2_ALG0_STAGE0_B0_ADDR ;; band 0:
        MOD_MIDEQ1_2_ALG0_STAGE0_B1_ADDR
        MOD_MIDEQ1_2_ALG0_STAGE0_B2_ADDR 
        MOD_MIDEQ1_2_ALG0_STAGE0_A1_ADDR 
        MOD_MIDEQ1_2_ALG0_STAGE0_A2_ADDR )          
   (fil MOD_MIDEQ1_2_ALG0_STAGE1_B0_ADDR ;; band 1:
        MOD_MIDEQ1_2_ALG0_STAGE1_B1_ADDR
        MOD_MIDEQ1_2_ALG0_STAGE1_B2_ADDR
        MOD_MIDEQ1_2_ALG0_STAGE1_A1_ADDR
        MOD_MIDEQ1_2_ALG0_STAGE1_A2_ADDR)
   (fil MOD_MIDEQ1_2_ALG0_STAGE2_B0_ADDR ;; band 2:
        MOD_MIDEQ1_2_ALG0_STAGE2_B1_ADDR
        MOD_MIDEQ1_2_ALG0_STAGE2_B2_ADDR
        MOD_MIDEQ1_2_ALG0_STAGE2_A1_ADDR
        MOD_MIDEQ1_2_ALG0_STAGE2_A2_ADDR)
   (fil MOD_MIDEQ1_2_ALG0_STAGE3_B0_ADDR ;; band 3:
        MOD_MIDEQ1_2_ALG0_STAGE3_B1_ADDR
        MOD_MIDEQ1_2_ALG0_STAGE3_B2_ADDR
        MOD_MIDEQ1_2_ALG0_STAGE3_A1_ADDR
        MOD_MIDEQ1_2_ALG0_STAGE3_A2_ADDR)
   (fil MOD_MIDEQ1_2_ALG0_STAGE4_B0_ADDR ;; band 4:
        MOD_MIDEQ1_2_ALG0_STAGE4_B1_ADDR
        MOD_MIDEQ1_2_ALG0_STAGE4_B2_ADDR
        MOD_MIDEQ1_2_ALG0_STAGE4_A1_ADDR
        MOD_MIDEQ1_2_ALG0_STAGE4_A2_ADDR)))


