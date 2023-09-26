#lang plai
(require "grammars.rkt")
(require "parser.rkt")

;Prueba para parse de más de 2
;(parse '{+ 1 2 3 4})
;(parse '{- 1 2 3 4})
;(parse '{* 1 2 3 4})
;(parse '{min 1 2 3 4})
;(parse '{max 1 2 3 4})


;Aridad de 1
;(parse '{Sub1 3})
;(parse '{Sub1 1 2})
;(parse '{Add1 3})
;(parse '{Add1 1 2})
;(parse '{not 1 2})



;Aridad de 2
;(parse '{/ 1 2 3 4})
;(parse '{mod 1 2 3 4})
;(parse '{anD 1 2 3 4})
;(parse '{expt 1 2 3 4})
;(parse '{sqrt 1 2 3 4)
;(parse '{> 1 2})
;(parse '{< 1 2})

;(parse '{<= 1 2})
;(parse '{>= 1 2})
;(parse '{= 1 2})
;(parse '{oR 1 2})