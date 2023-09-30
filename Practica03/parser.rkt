#lang plai
(require "grammars.rkt")
;Función list-to-binding que convierte una lista en bindings (pares de variables con sus
;respectivos valores)
(define (list-to-binding ls)
  (cond
    [(empty? ls) (error 'list-to-binding "La lista ingresada es vacía")]
    [(not(=(length ls)2)) (error 'list-to-binding "No se puede hacer binding por cantidad de
elementos distinta a 2")]
    [else (binding (car ls)(parse (second ls)))]))

; Función parse que analiza sintácticamente que las expresiones estén escritas de forma correcta
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
             (error 'parse
                    (format "La operación sub1 debe ser ejecutada con 1 argumentos.")))]
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
             (error 'parse
                    (format "La operación min debe ser ejecutada con mas de 0 argumentos." )))]
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



