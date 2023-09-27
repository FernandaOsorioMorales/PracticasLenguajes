   #lang plai

(require "grammars.rkt")
(require "parser.rkt")

(define (interp expr)
  (type-case WAE expr
    [id (i) (error 'interp "Variable libre.")]
    [num (n) n]
    [bool (b) b]
    [op(f args) (foldr f (car args) (cdr args))]
    [else (error 'interp"")]))

(define (subst expr sub-id val)
  (type-case WAE expr
    [id (i) (if (symbol=? i sub-id id) val expr)]
    [num (n) expr]
    [bool (b) (bool b)]
    [op (f args) (op f (map (lambda (x) (subst sub-id val x)) args))]
    [with (asings body) (subst-with2 asings body sub-id val)]
    [else (error 'i "")]))


;(define (subst-with assings body sub-id val)
;  (let ([index-of-id (index-where assings (lambda (x) (type-case Binding assings [binding (i valor) (symbol=? i sub-id)]) assings))])
;    (if (not index-of-id) (with (map (lambda (x) (type-case Binding assings [binding (i valor) (subst   )])) assings)
;    (list-update assings index-of-id (lambda (x) (type-case Binding assings [binding (i valor)      ])))))))
  


(define (subst-with2 assings body sub-id val)
  (if (empty? (filter (lambda (x) (type-case Binding assings [binding (i valor) (symbol=? i sub-id)])) assings))
      (with (map (lambda (x) (type-case Binding assings [binding (i valor) (binding i (subst valor sub-id val))])) assings) (subst body sub-id val))
      (with (map (lambda (x) (type-case Binding assings [binding (i valor) (binding i (subst valor sub-id val))])) assings) body)))
  

  
  ;(let ([variable (findf (lambda (x) (type-case Binding assings [binding (i valor) (symbol=? i sub-id)]) assings))]) ()))
  ;(if () ( with id (subst value sub-id val) body ) ( with id ( subst value sub-id val ) ( subst body sub-id val)))



