#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "desugar.rkt")

(define-type CFSBAE-Val
  [num-v (n number?)]
  [bool-v (b boolean?)]
  [string-v (s string?)]
  [closure-v (args (listof symbol?)) (body CFSBAE?) (env Env?)])

(define-type Env
  [mt-env]
  [cons-env (id symbol?) (value CFSBAE-Val?) (rest-env Env?)])

(define (interp expr env)
    (match expr
      [(num n) (num-v n)]
      [(bool b) (bool-v b)]
      [(strinG s) (string-v s)]
      [(id x) (lookup x env)]
      [(fun param body) (closure-v param body env)]
      [(op f args) (interp (desugar (parse (apply f (value-list (map (lambda (e) (interp e env)) args))))) mt-env)]
      [(app f args)
         (let ([fun-val (interp f env)])
           (interp (closure-v-body fun-val)  (get-env (closure-v-args fun-val) args (closure-v-env fun-val))))]
      [(iF test then other) (interp-if (interp test env) then other env)]
))


(define (get-env vars args env)
  (if (= (length vars) (length args))
      (add-to-env vars args env)
      (error 'interp "Numero de argumentos y parametros distinto")))

(define (add-to-env vars args env)
  (if (empty? vars)
     env
     (cons-env (car vars) (interp (car args) env) (add-to-env (cdr vars) (cdr args) env))))

(define (value-list args)
  (if (empty? args) empty
      (type-case CFSBAE-Val (car args)
        [num-v (n) (cons n (value-list (cdr args)))]
        [bool-v (b) (cons b (value-list (cdr args)))]
        [string-v (s) (cons s (value-list (cdr args)))]
        [else error 'interp "Error al obtener el valor"])))

(define (interp-if test then other env)
  (type-case CFSBAE-Val test
    [bool-v (v) (if v (interp then env) (interp other env))]
    [else (error 'interp "interp: La condición de una expresión if debe ser un booleano.")]))

(define (lookup sub-id env)
  (type-case Env env
    [mt-env () (error 'interp (format "Variable libre ~a" sub-id))] 
    [cons-env (id val renv) (if (symbol=? id sub-id) val (lookup sub-id renv))]
  ))