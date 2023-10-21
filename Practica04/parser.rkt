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
                     (conDS (map parse-conditions (take (cdr s-exp) (sub1 (length (cdr s-exp)))))
                            (parse (second other)))))]
       [(with)  (withS (parseo-bindings-normal (second s-exp)) (parse (third s-exp)))];fin del caso del with
       [(with*) (with*S (parseo-bindings-estrellita (second s-exp)) (parse (third s-exp)))]; fin del caso with*
         )))


;(parse-condition '{#t 10})
(define (parse-condition cdn)
  (condition (parse (first cdn))
             (parse (second cdn)))) 


(define (parse-conditions conds)
  (if (empty? conds)
      empty
      (cons (parse-condition (first conds))
            (parse-condition (second conds)))))



;Función que parsea bindings cuando se encuentran en un with convencional
       (define (parseo-bindings-normal ls-bindings)
         (let ([comparador (lambda (x y) (symbol=? (first x) (first y)))])
    (if (boolean? (hayDuplicados? ls-bindings comparador))
        (map (lambda (parseoVar) (binding (first parseoVar) (parse (cadr parseoVar))))
             ls-bindings)
          (error 'parseo-bindings-normal "parse: El identificador x está declarado más de una vez"))) 
         )
     ; Función que parsea bindings cuando se encuentran en un with estrellita
       (define (parseo-bindings-estrellita ls-bindings)
         (map (lambda (parseoVar) (binding (first parseoVar) (parse (cadr parseoVar))))
           ls-bindings)
         )
       ; Función auxiliar que se encarga de ver si hay variables duplicadas en una lista
       (define (hayDuplicados? lst comparador) 
         (cond
           [(empty? lst) #f]
           [(estaVariable? (first lst) (cdr lst) comparador) (first lst)]
           [else (hayDuplicados? (cdr lst) comparador)]))

      ; Función que nos ayuda a verficar si una variable se encuentra en una lista 
      (define (estaVariable? e lst comparador)
        (cond
          [(empty? lst) #f]
          [(comparador (first lst) e) #t]
          [else (estaVariable? e (cdr lst) comparador)]))