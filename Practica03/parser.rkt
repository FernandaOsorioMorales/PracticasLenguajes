#lang plai

(require "grammars.rkt")
(define (list-to-binding ls)
  (binding (car ls) (parse (cdr ls)) ))

(define (parse s-exp)
  (cond
    [(number? s-exp) (num s-exp)]
    [(symbol? s-exp) (id s-exp)]
    [(boolean? s-exp) (bool s-exp)]
    [(list? s-exp) (let ([head (car s-exp)])
                     (case head
                       [(+) (op + (map parse (cdr s-exp)))]
                       [(-) (op - (map parse (cdr s-exp)))]
                       [(*) (op * (map parse (cdr s-exp)))]
                       [(/) (op / (map parse (cdr s-exp)))]
                       [(mod) (op modulo (map parse (cdr s-exp)))]
                       [(min) (op min (map parse (cdr s-exp)))]
                       [(max) (op max (map parse (cdr s-exp)))]
                       [(expt) (op expt (map parse (cdr s-exp)))]
                       [(sqrt) (op sqrt (map parse (cdr s-exp)))]
                       [(Sub1) (if (= (length(cdr s-exp))1) (op sub1 (map parse (cdr s-exp))) (error 'Sub1 "Número de argumentos incorrecto"))]
                       [(Add1) (if (= (length(cdr s-exp))1) (op add1 (map parse (cdr s-exp))) (error 'Sub1 "Número de argumentos incorrecto"))]
                      [(<) (op < (map parse (cdr s-exp)))]
                      [(>) (op > (map parse (cdr s-exp)))]
                      [(<=) (op <= (map parse (cdr s-exp)))]
                      [(>=) (op >= (map parse (cdr s-exp)))]
                      [(=) (op = (map parse (cdr s-exp)))]
                      [(not) (op not(map parse (cdr s-exp)))]
                      [(anD) (op anD (map parse (cdr s-exp)))]
                      [(oR) (op oR (map parse (cdr s-exp)))]
                      [(Zero?) (op zero? (map parse (cdr s-exp)))]
                      [(Num?) (op num? (map parse (cdr s-exp)))]
                      [(Str?) (op string? (map parse (cdr s-exp)))]
                      [(Bool?) (op boolean? (map parse (cdr s-exp)))]
                      [(str-length) (op string-length (map parse (cdr s-exp)))]
                       [(with*) (let* ([vars (second s-exp)]
                                  [bindings (map list-to-binding
                                                 vars)])
                                (with* bindings
                                       parse (third s-exp)))]))]))

