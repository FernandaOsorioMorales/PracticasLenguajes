#lang plai

(require "grammars.rkt")

;; s-expression -> TCFWSBAE
(define (parse s-exp)
  (cond
    [(symbol? s-exp) (id s-exp)]
    [(number? s-exp) (num s-exp)]
    [(boolean? s-exp) (bool s-exp)]
    [(string? s-exp) (strinG s-exp)]
    [(list? s-exp)
     (case (first s-exp)
       ;aridad 1
       [(add1 sub1 not zero? num? str? bool? str-length)
        (if (= (length (cdr s-exp)) 1)
            (cond
              [(equal? (car s-exp) 'str?) (op (eval 'string? (make-base-namespace)) (map parse (cdr s-exp)))]
              [(equal? (car s-exp) 'str-length) (op (eval 'string-length (make-base-namespace)) (map parse (cdr s-exp)))]
              [(equal? (car s-exp) 'num?) (op (eval 'number? (make-base-namespace)) (map parse (cdr s-exp)))]
              [(equal? (car s-exp) 'bool?) (op (eval 'boolean? (make-base-namespace)) (map parse (cdr s-exp)))]
              [else (op (eval (car s-exp) (make-base-namespace)) (map parse (cdr s-exp)))])
            (error 'parse (format "Error.La operación ~a debe tener 1 argumento" (car s-exp))))]
       ;caso modulo y expt
       [(modulo expt)
        (if (= (length (cdr s-exp)) 2)
            (op (eval (car s-exp) (make-base-namespace)) (map parse (cdr s-exp)))
            (error 'parse (format "Error. La operación ~a debe tener 2 argumentos" (car s-exp))))]
       ;aridad mayor a 0
       [(+ - / * min max sqrt < > <= >= = and or)
        (if (> (length (cdr s-exp)) 0)
            (cond
              [(equal? (car s-exp) 'or) (op oR (map parse (cdr s-exp)))]
              [(equal? (car s-exp) 'and) (op anD (map parse (cdr s-exp)))]
              [else (op (eval (car s-exp) (make-base-namespace)) (map parse (cdr s-exp)))])
            (error 'parse (format "Error. La operación ~a debe tener más de 2 argumentos." (car s-exp))))]
       ;caso función
       [(fun) (fun (parseo-funcion (second s-exp)) (queTipo(fourth s-exp)) (parse (fifth s-exp)))]
       ;caso if
       [(if) (if (equal? (length s-exp) 4)
             (iF (parse (second s-exp)) (parse (third s-exp)) (parse (fourth s-exp)))
            (error 'parse "Le faltan argumentos al if"))]
       ;caso with
       [(with) (with (parseo-with (second s-exp)) (parse (third s-exp)))]
       ;caso with estrellita
       [(with*) (with* (parseo-with-estrellita (second s-exp)) (parse (third s-exp)))]
       ;caso cond
       [(cond) (conD (parseo-condicional (drop-right (cdr s-exp) 1)) (parse (last s-exp)))]
       ;caso else
       [(else) (parse (second s-exp))]
       ;aplicacion de función
       [else (app (parse (first s-exp)) (map parse (cdr s-exp)))])]))

;Función auxiliar parseo cond
(define (parseo-condicional condiciones)
  (map (lambda (condi) (condition (parse (first condi)) (parse (last condi))))
       condiciones))

;Función auxiliar parseo funciones
(define (parseo-funcion parametros)
  (if (empty? parametros)
      '()
      (if (estaVariable? (car parametros) (cdr parametros) equal?)
          (error 'parse "Parametro ~a repetido" (car parametros))
          (cons (param (caar parametros) (queTipo (last (car parametros)))) (parseo-funcion (cdr parametros))))))

;Función para verificar el tipo
(define (queTipo arg)
  (cond
    [(equal? arg 'num) (numberT)]
    [(equal? arg 'bool) (booleanT)]
    [(equal? arg 'str) (stringT)]
    [else (funT (tipoFuncion arg))]))

;Función auxiliar de tipo
(define (tipoFuncion arg)
  (if (empty? arg)
      '()
      (case (car arg)
        [(->) (tipoFuncion (cdr arg))]
        [else (cons (queTipo (car arg)) (tipoFuncion (cdr arg)))])))

;Función auxiliar parseo with normal
(define (parseo-with bindings)
  (let ([comparador (lambda (x y) (symbol=? (first x) (first y)))])
    (if (boolean? (hayDuplicados? bindings comparador))
        (parseo-with bindings)
        (error 'parse-bindings "Hay un identificador duplicado"))))

;Función auxiliar with estrellita
(define (parseo-with-estrellita bindings)
  (map (lambda (bede)
         (binding (first bede) (queTipo (third bede)) (parse (last bede))))
       bindings))
;Función auxiliar para verificar si se tiene un elemento duplicado en la lista 
(define (hayDuplicados? lista comparador) 
  (cond
    [(empty? lista) #f]
    [(estaVariable? (first lista) (cdr lista) comparador) (first lista)]
    [else (hayDuplicados? (cdr lista) comparador)]))

;; Función para verificar la pertenencia de un elemento en una lista
(define (estaVariable? elemento lista comparador)
  (cond
    [(empty? lista) #f]
    [(comparador (first lista) elemento) #t]
    [else (estaVariable? elemento (cdr lista) comparador)]))

