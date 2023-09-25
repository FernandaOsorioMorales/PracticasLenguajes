#lang plai

(define-type Binding
  [binding (id symbol?) (value WAE?)])

;Ejercicio 1)
;Extender la definicion de la gramatica del lengueje WAE
(define-type WAE
  [id (i symbol?)]
  [bool (b boolean?)]
  [strinG (s string?)]
  [num (n number?)]
  [op (f procedure?) (args (listof WAE?))]
  [with (assigns (listof Binding?)) (body WAE?)]
  [with* (assigns (listof Binding?)) (body WAE?)])

;Ejercicio 2)
;anD :: n args -> booolean
;Funcion que recibe n argumentos y devulve el
;resultado de aplicar and
(define (anD . args)
  (if (null? args)
      #t
      (if (car args)
          (apply anD (cdr args))
          #f)))

;oR :: n args -> boolean
;Funcion que recibe n argumentos y devulve el
;resultado de aplicar or
(define (oR . args)
  (if (null? args)
      #f
      (if (car args)
          #t
          (apply oR (cdr args)))))
