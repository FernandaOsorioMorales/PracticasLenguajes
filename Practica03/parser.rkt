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
                       [(anD) (op anD (map parse (cdr s-exp)))]
                       [(with*) (let* ([vars (second s-exp)]
                                  [bindings (map list-to-binding
                                                 vars)])
                                (with* bindings
                                       parse (third s-exp)))]))]))

