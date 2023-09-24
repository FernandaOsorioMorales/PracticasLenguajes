#lang plai

(require "grammars.rkt")

(define (parse s-exp)
  (cond
    [(number? s-exp) (num s-exp)]
    [(symbol? s-exp) (id s-exp)]
    [(boolean? s-exp) (bool s-exp)]
    [(list? s-exp) (let ([head (car s-exp)])
                     (case head
                       [(+) (op + (map parse (cdr s-exp)))]
                       [(-) (op - (map parse (second s-exp)))]
                       ;[(and) (op and (map parser (cdr s-exp)))]
                       [(with) (let ([assign (second s-exp)])
                                 (with (first assign)
                                       (parse (second assign))
                                       (parse (third s-exp))))]))]))
