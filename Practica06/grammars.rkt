#lang plai

(define-type Type
  [numberT]
  [booleanT]
  [stringT]
  [funT (params (listof Type?))])

(define-type TypeContext
  [phi]
  [gamma (id symbol?) (type Type?) (rest TypeContext?)])

(define-type Binding
  [binding (id symbol?) (type Type?) (value TCFWSBAE?)])

(define-type Param
  [param (id symbol?) (type Type?)])

(define-type Condition
  [condition (test-expr TCFWSBAE?) (then-expr TCFWSBAE?)])

  
(define-type TCFWSBAE
  [id (i symbol?)]
  [num (n number?)]
  [bool (b boolean?)]
  [strinG (s string?)]
  [iF (test-expr TCFWSBAE?) (then-expr TCFWSBAE?) (else-expr TCFWSBAE?)]
  [conD (conditions (listof Condition?)) (else-expr TCFWSBAE?)]
  [op (f procedure?) (args (listof TCFWSBAE?))]
  [fun (params (listof Param?)) (return-type Type?) (body TCFWSBAE?)]
  [app (f TCFWSBAE?) (args (listof TCFWSBAE?))]
  [with (bindings (listof Binding?)) (body TCFWSBAE?)]
  [with* (bindings (listof Binding?)) (body TCFWSBAE?)])

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