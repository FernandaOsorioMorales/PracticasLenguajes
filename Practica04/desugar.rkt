#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; desugar :: CFWSBAE -> CFSBAE
(define (desugar expr)
  (type-case CFWSBAE expr
    [numS (n) (num n)]
    [idS (x) (id x)]
    [boolS (b) (bool b)]
    [strinGS (s) (strinG s)]
    [opS (f args) (op f (map (lambda (a) (desugar a)) args))]
    [funS (param body) (fun param (desugar body))]
    [with*S (bs bo) (desugar (toWiths bs bo))]
    [withS (bs bo)
          (let ([pairBS (bsParapar bs (cons '() '()))])
            (app (fun (reverse (car pairBS)) (desugar bo)) (reverse (cdr pairBS))))]
    [appS (f args) (app (desugar f) (map (lambda (a) (desugar a)) args))]
    [iFS (test then else) (iF (desugar test) (desugar then) (desugar else))]
    [conDS (conds else) (desugar (condsToIfs conds else))]))




(define (condsToIfs conds else)
  (match conds
    ['() else]
    [(cons x xs) (type-case Condition x
        [condition (test then) (iFS test then (condsToIfs xs else))])]))

(define (toWiths bs bo)
  (match bs
    ['() bo]
    [(cons x xs) (withS (list x) (toWiths xs bo))]))
            
(define (bsParapar bs acumulador)
  (if (empty? bs)
      acumulador
      (type-case Binding (car bs)
        [binding (id value) (bsParapar (cdr bs) (cons (cons id (car acumulador))
                                  (cons (desugar value) (cdr acumulador
                                                             ))))])))