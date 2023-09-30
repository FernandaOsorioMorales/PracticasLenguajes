#lang plai
(require "grammars.rkt")
(define (list-to-binding ls)
  (cond
    [(empty? ls) (error 'list-to-binding "La lista ingresada es vacía")]
    [(not(=(length ls)2)) (error 'list-to-binding "No se puede hacer binding por cantidad de
elementos distinta a 2")]
    [else (binding (car ls)(parse (second ls)))]))

(define (parse s-exp)
  (cond
    [(number? s-exp) (num s-exp)]
    [(symbol? s-exp) (id s-exp)]
    [(string? s-exp) (strinG s-exp)]
    [(boolean? s-exp) (bool s-exp)]
    [(list? s-exp)
     (let ([head (car s-exp)])
       (case (first s-exp)
         ;De aridad 1
         [(sub1 add1 not )
          (if(=(length (cdr s-exp))1)
             (op (eval head (make-base-namespace))( map parse (cdr s-exp)))
             (error 'argumentos-incorrectos
                    (format "Se espera 1 argumento, y se han recibido ~a"  (length (cdr s-exp)))))]
         ;Caso zero?
         [(zero?) (if (= (length (cdr s-exp)) 1)
           (op zero? (map parse (cdr s-exp)))
           (error 'argumentos-incorrectos
                  (format "Se espera 1 argumentos, y se han recibido ~a" (length (cdr s-exp)))))]
         ;Caso num?
         [(num?) (if (= (length (cdr s-exp)) 1)
           (op number? (map parse (cdr s-exp)))
           (error 'argumentos-incorrectos
                  (format "Se espera 1 argumentos, y se han recibido ~a" (length (cdr s-exp)))))]
         ;Caso str?
         [(str?) (if (= (length (cdr s-exp)) 1)
           (op string? (map parse (cdr s-exp)))
           (error 'argumentos-incorrectos
                  (format "Se espera 1 argumentos, y se han recibido ~a" (length (cdr s-exp)))))]
         ;Caso bool?
         [(bool?) (if (= (length (cdr s-exp)) 1)
           (op boolean? (map parse (cdr s-exp)))
           (error 'argumentos-incorrectos
                  (format "Se espera 1 argumentos, y se han recibido ~a" (length (cdr s-exp)))))]
         ;Caso str-length
         [(str-length) (if (= (length (cdr s-exp)) 1)
           (op string-length (map parse (cdr s-exp)))
           (error 'argumentos-incorrectos
                  (format "Se espera 1 argumentos, y se han recibido ~a" (length (cdr s-exp)))))]
         
         ;De aridad 2
         [(expt modulo )
          (if(=(length (cdr s-exp))2)
             (op (eval head (make-base-namespace))( map parse (cdr s-exp)))
             (error 'argumentos-incorrectos
                    (format "Se esperan 2 argumentos, y se han recibido ~a"  (length (cdr s-exp)))))]
         ;Se espera que sean mas de 0 argumentos
         [(+ - * / min max sqrt < > <= >= = )
          (if(>(length (cdr s-exp))0)
             (op (eval head (make-base-namespace))( map parse (cdr s-exp)))
             (error 'argumentos-incorrectos
                    (format "Se esperan mas de 0 argumentos, y se han recibido ~a" (length (cdr s-exp)))))]
         ;Caso de and
         [(and) (if (> (length (cdr s-exp)) 0)
           (op anD (map parse (cdr s-exp)))
           (error 'argumentos-incorrectos
                  (format "Se esperan mas de 0 argumentos, y se han recibido ~a" (length (cdr s-exp)))))]
         ;Caso de or
         [(or) (if (> (length (cdr s-exp)) 0)
           (op oR (map parse (cdr s-exp)))
           (error 'argumentos-incorrectos
                  (format "Se esperan mas de 0 argumentos, y se han recibido ~a" (length (cdr s-exp)))))]

         ;Los casos del with
         [(with)  (with (parseo-bindings-normal (second s-exp)) (parse (third s-exp)))];fin del caso del with
         [(with*) (with* (parseo-bindings-estrellita (second s-exp)) (parse (third s-exp)))]; fin del caso with*
         ))]
    ))

       (define (parseo-bindings-normal ls-bindings)
         (let ([comparador (lambda (x y) (symbol=? (first x) (first y)))])
    (if (boolean? (hayDuplicados? ls-bindings comparador))
        (map (lambda (parseoVar) (binding (first parseoVar) (parse (cadr parseoVar))))
             ls-bindings)
          (error 'parseo-bindings-normal "Hay un identificador duplicado"))) 
         )

       (define (parseo-bindings-estrellita ls-bindings)
         (map (lambda (parseoVar) (binding (first parseoVar) (parse (cadr parseoVar))))
           ls-bindings)
         )

       (define (hayDuplicados? lst comparador) 
         (cond
           [(empty? lst) #f]
           [(estaVariable? (first lst) (cdr lst) comparador) (first lst)]
           [else (hayDuplicados? (cdr lst) comparador)]))

      (define (estaVariable? e lst comparador)
        (cond
          [(empty? lst) #f]
          [(comparador (first lst) e) #t]
          [else (estaVariable? e (cdr lst) comparador)]))



