#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; desugar :: CFWSBAE -> CFSBAE
(define (desugar expr)
  (type-case CFWSBAE expr
    [numS (n) (num n)]
    [idS (i) (id i)]
    [boolS (b) (bool b)]
    [strinGS (s) (strinG s)]
    [opS (f args) (op f (map (lambda (a) (desugar a)) args))]
    [funS (param bo) (fun param (desugar bo))]
    [with*S (bs bo) (desugar (with*S-to-withS bs bo))]
    [withS (bs bo)
          (let ([pairBS (bs-to-pair bs (cons '() '()))])
            (app (fun (reverse (car pairBS)) (desugar bo)) (reverse (cdr pairBS))))]
    [appS (f args) (app (desugar f) (map (lambda (a) (desugar a)) args))]
    [iFS (test then else) (iF (desugar test) (desugar then) (desugar else))]
    [conDS (conds else) (desugar (conDS-to-iFS conds else))]))

(define (conDS-to-iFS conds else)
  (if (empty? conds)
      else
      (type-case Condition (car conds)
        [condition (test then) (iFS test then (conDS-to-iFS (cdr conds) else))])))

(define (with*S-to-withS bs bo)
  (cond
    [(empty? bs) bo]
    [else (withS (list (car bs)) (with*S-to-withS (cdr bs) bo))]))
            
(define (bs-to-pair bs acc)
  (if (empty? bs)
      acc
      (type-case Binding (car bs)
        [binding (id value) (bs-to-pair (cdr bs) (cons (cons id (car acc))
                                  (cons (desugar value) (cdr acc))))])))