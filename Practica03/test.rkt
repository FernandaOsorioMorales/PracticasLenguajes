#lang plai
(require "grammars.rkt")
(require "parser.rkt")

;Pruebas que funcionan
;Parse
;Aridad de 1
;(parse '{sub1 3})
(parse '{sub1 2})
;(parse '{add1 3})
(parse '{add1 1 })

;Aridad de 2
(parse '{expt 1 2 })

;Prueba para argumentos con numero mayor a 0
(parse '{+ 1 2})
;(parse '{- 1 2 3 4})
;(parse '{* 1 2 3 4})
;(parse '{/ 1 2 3 4})
(parse '{min 1 2 3 4})
;(parse '{max 1 2 3 4})
;(parse '{sqrt 1 2 3 4)
;(parse '{> 1 2})
;(parse '{< 1 2})

;(parse '{<= 1 2})
;(parse '{>= 1 2})
(parse '{= 1 2})
(parse '{or #t #f})
(parse '{and 4 3})
(parse '{with {{x 5}} {+ 3 8}})
;(parse '{with ({x "Hello"}) {str-length x}})




;Subst

;Interp


;Pruebas que arrojan un error
;;Parse
;Aridad 1
;(parse '{not 1 2})

;(parse '{mod 1 2 3 4})

;Subst

;Interp
