#lang plai
(require "grammars.rkt")

;; parse :: s-exp -> RCFSBAE
(define (parse s-exp)
  (cond
    [(number? s-exp) (num s-exp)]
    [(boolean? s-exp) (bool s-exp)]
    [(string? s-exp) (strinG s-exp)]
    [(symbol? s-exp) (id s-exp)]
    [(list? s-exp) (parse-ls s-exp)]))

;Función parse
(define (parse-ls s-exp)
  (let([head (first s-exp)] [rst (rest s-exp)])
    (case head
      ;Aridad 1
      [(sub1 add1 not)
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
         [(expt modulo)
          (if(=(length (cdr s-exp)) 2)
             (op (eval head (make-base-namespace))( map parse (cdr s-exp)))
             (error 'argumentos-incorrectos
                    (format "Se esperan 2 argumentos, y se han recibido ~a"  (length (cdr s-exp)))))]
         ;Se espera que sean mas de 0 argumentos
         [(+ - * / min max sqrt < > <= >= = )
          (if(>(length (cdr s-exp))0)
             (op (eval head (make-base-namespace)) (map parse (cdr s-exp)))
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
      ;Caso función
      [(fun) (parse-fun rst)]
      ;Caso if
      [(if) (parse-if rst)]
      ;Caso cond
      [(cond) (if (<= (length (cdr s-exp)) 1)
                  (error 'parse "parse: La expresión conds debe contar con 1 o mas condiciones y una expresión else.")
                  (let [(other (last s-exp))]
                     (cond (map parse-condition (take (cdr s-exp) (sub1 (length (cdr s-exp)))))
                            (parse (second other)))))]
      ;Caso with
       [(with)  (with (parseo-bindings-normal (second s-exp)) (parse (third s-exp)))]
      ;Caso with*
       [(with*) (with* (parseo-bindings-estrellita (second s-exp)) (parse (third s-exp)))]
      ;Caso rec
       [(rec) (rec (parseo-bindings-rec (second s-exp)) (parse (third s-exp)))]
      ;caso else
       [else (parse-apps s-exp)]
)))

;Función auxiliar para parsear bindings en expresiones recursivas
(define (parseo-bindings-rec ls-bindings)
     (map (lambda (parseoVar) (binding (first parseoVar) (parse (cadr parseoVar))))
           ls-bindings))
;Función auxiliar del parseo de aplicación de funciones
(define (parse-apps apps)
  (app (parse (first apps)) (map parse (cdr apps))))

;Función auxiliar del parseo con funciones
(define (parse-fun rst)
  (if (check-duplicates (first rst))
      (error 'parse (format "Parámetro '~a definido dos veces." (check-duplicates (first rst))))
      (fun (first rst) (parse (second rst)))))

; Parse if
(define (parse-if rst)
  (match rst
    ['() (error 'parse "parse: No se provio de cuerpo de if.")]
    [(list a) (error 'parse "parse: Solo se provio la condicion, no las expresiones del if.")]
    [(list a b) (error 'parse "parse: Falta la else-expression.")]
    [(list a b c) (iF (parse a) (parse b) (parse c))]
    [else (error 'parse "parse: Expresiones de mas proveidas.")]))

;Función auxiliar para el parseo de cond
(define (parse-condition cdn)
  (condition (parse (first cdn))
             (parse (second cdn)))) 


;Función que parsea bindings cuando se encuentran en un with convencional
 (define (parseo-bindings-normal ls-bindings)
         (let ([comparador (lambda (x y) (symbol=? (first x) (first y)))])
           (if (boolean? (hayDuplicados? ls-bindings comparador))
               (map (lambda (parseoVar) (binding (first parseoVar) (parse (cadr parseoVar))))ls-bindings)
               (error 'parseo-bindings-normal "parse: El identificador x está declarado más de una vez"))) 
         )
;Función que parsea bindings cuando se encuentran en un with estrellita
 (define (parseo-bindings-estrellita ls-bindings)
    (map (lambda (parseoVar) (binding (first parseoVar) (parse (cadr parseoVar))))ls-bindings)
         )
;Función auxiliar que se encarga de ver si hay variables duplicadas en una lista
(define (hayDuplicados? lst comparador) 
         (cond
           [(empty? lst) #f]
           [(estaVariable? (first lst) (cdr lst) comparador) (first lst)]
           [else (hayDuplicados? (cdr lst) comparador)]))

;Función que nos ayuda a verficar si una variable se encuentra en una lista 
(define (estaVariable? e lst comparador)
   (cond
      [(empty? lst) #f]
      [(comparador (first lst) e) #t]
      [else (estaVariable? e (cdr lst) comparador)]))