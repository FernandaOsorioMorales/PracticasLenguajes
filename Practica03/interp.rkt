#lang plai

(require "grammars.rkt")
(require "parser.rkt")

(define (interp expr)
  (type-case WAE expr
    [id (i) (error 'interp "Variable libre.")]
    [num (n) n]
    [bool (b) b]
    ;[strinG (s) s]
    [op(f args) (apply f (map (lambda (x) (interp x)) args))]
    [with (bs bo) (interp (subst-list-bindings bs bo))]
    [with* (bs bo) (interp (toWith (with* bs bo)))]
    [else (error 'interp"")]))

(define (subst expr sub-id val)
  (type-case WAE expr
    [id (i) (if (symbol=? i sub-id id) val expr)]
    [num (n) expr]
    [bool (b) (bool b)]
    [op (f args) (op f (map (lambda (x) (subst x sub-id val)) args))]
    [with (asings body) (subst-withAlt asings body sub-id val)]
    [with* (asings body) (subst (toWith expr) sub-id val)]
    [else (error 'i "")]))

(define (subst-withAlt ass body sub-id val)
  (if (empty? (filter (lambda (x) (type-case Binding ass [binding (i valor) (symbol=? i sub-id)])) ass))
      (with  (substBindings sub-id val ass) (subst body sub-id val))
      (with (substBindings sub-id val ass) body)))
 
(define (substBindings sub-id val bs)
  (match  bs
    ['() empty]
    [(cons x xs) (cons (binding (binding-id x) (subst (binding-value x) sub-id val)) (substBindings sub-id val xs))]))

(define (subst-list-bindings bs bo)
  (if (empty? bs) bo
      [let ([head (car bs)])
        (subst-list-bindings (cdr bs) (subst bo (binding-id head) (binding-value head)))]))


(define (toWith w)
  (type-case WAE with*
     [with* (ass body)
            (cond
              [(empty? ass) body]
              [else (with (list (car ass) (toWith (with* (cdr ass) body))))])]
     [else (error "No es with*")]))