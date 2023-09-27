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

(define (subst sub-id value expr)
  (type-case WAE expr
    [id (i) (if (symbol=? i sub-id id)
                value
                (id i))]
    [bool (b) (bool b)]
    [num (n) (num n)]
    [op (f args) (op f (map (lambda (x) (subst sub-id value x)) args))]
    [else (error 'i "")]))
