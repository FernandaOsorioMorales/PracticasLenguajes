#lang plai

(require "grammars.rkt")

;; parse :: s-exp -> CFWSBAE
(define (parse s-exp)
  (cond
    [(number? s-exp) (numS s-exp)]
    [(boolean? s-exp) (boolS s-exp)]
    [(string? s-exp) (strinGS s-exp)]
    [(symbol? s-exp) (idS s-exp)]
    [(list? s-exp) (parse-ls s-exp)]))

(define (parse-ls s-exp)
  (let([head (first s-exp)] [rst (rest s-exp)])
    (case head
      [(fun) (funS (first rst) (parse (second rst)))]
      [(if) (iFS (parse (first rst))
                 (parse (second rst))
                 (parse (third rst)))]
      [(cond) (if (<= (length (cdr s-exp)) 1)
                  (error 'parse "parse: La expresión conds debe contar con 1 o mas condiciones y una expresión else.")
                  (let [(other (last s-exp))]
                     (conDS (map parse-condition (take (cdr s-exp) (sub1 (length (cdr s-exp)))))
                            (parse (second other)))))])))

;(parse-condition '{#t 10})
(define (parse-condition cdn)
  (condition (parse (first cdn))
             (parse (second cdn)))) 


(define (parse-conditions conds)
  (if (empty? conds)
      empty
      (cons (parse-condition (first conds))
            (parse-condition (second conds)))))