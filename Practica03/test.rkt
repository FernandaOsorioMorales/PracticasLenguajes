#lang plai
(require "grammars.rkt")
(require "parser.rkt")

;Prueba para parse de add 
(parse '{+ 1 2 3 4})


(parse '{- 1 2 3 4})

(parse '{anD 1 2 3 4})