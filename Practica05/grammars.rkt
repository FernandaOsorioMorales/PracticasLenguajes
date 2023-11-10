#lang plai

(define-type Binding
  [binding (id symbol?) (value RCFSBAE?)])


(define-type Condition
  [condition (test-expr RCFSBAE?) (then-expr RCFSBAE?)])


(define-type RCFSBAE
  [num (n number?)]
  [id (i symbol?)]
  [bool (b boolean?)]
  [strinG (s string?)]
  [op (f procedure?) (args (listof RCFSBAE?))]
  [with (bindings (listof Binding?)) (body RCFSBAE?)];;
  [with* (bindings (listof Binding?)) (body RCFSBAE?)];;
  [fun (params (listof symbol?)) (body RCFSBAE?)]
  [app (f RCFSBAE?) (args (listof RCFSBAE?))]
  [iF (test-expr RCFSBAE?) (then-expr RCFSBAE?) (else-expr RCFSBAE?)]
  [rec (bindings (listof Binding?)) (body RCFSBAE?)])

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