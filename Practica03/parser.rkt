#lang plai

(require "grammars.rkt")

(define (parse s-exp)
  (cond
    [(number? s-exp) (num s-exp)]
    [(symbol? s-exp) (id s-exp)]
    [(boolean? s-exp) (bool a-exp)]
    [(list? s-exp) (let ([head (car s-exp)])
                     (case head
                       [(+) (add (parse (second s-exp))
                                 (parse (third s-exp)))]
                       [(-) (sub (parse (second s-exp))
                                 (parse (third s-exp)))]
                       ;[(and) (op and (map parser (cdr s-exp)))]
                       [(with) (let ([assign (second s-exp)])
                                 (with (first assign)
                                       (parser (second assign))
                                       (parser (third s-exp))))]))]))
