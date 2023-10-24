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

;; interp :: CFSBAE x Env -> CFSBAE-Val
(define (interp expr env)
    (match expr
      [(num n) (num-v n)]
      [(bool b) (bool-v b)]
      [(strinG s) (string-v s)]
      [(id x) (lookup x env)]
      [(fun param body) (closure-v param body env)]
      [(op f args) (apply f (map (lambda (x) (interp x env)) args))]
      ;;[(app f args) (interp-app f args env)]
      [(app fun-expr arg)
       (let ([fun-val (interp fun-expr mt-env)])
         (interp
          (closure-v-body fun-val)
          (cons-env (closure-v-args fun-val) (interp arg) (closure-v-env fun-val))))]
      [(iF test then other) (interp-if (interp test env) then other env)]
))


(define (interp-app f args env)
  (match f [(fun fargs body) (interp-app-aux fargs args body env)]))


(define (interp-app-aux fargs args body env)
  (if (= (length fargs) (length args))
      (interp body (adds-to-env fargs args env))
      (error 'interp "Numero de argumentos y parametros distinto")))


(define (interp-if test then other env)
  (match test
    [(bool-v v) (if v (interp then env) (interp other env))]
    [else (error 'interp "interp: La condición de una expresión if debe ser un booleano.")]))


;; lookup :: symbol x Env -> CFSBAE-Val
(define (lookup sub-id env)
  (match env
    [mt-env (error 'interp (format "Variable libre ~a" sub-id))] 
    [(cons-env id val renv) (if (symbol=? id sub-id) val (lookup sub-id renv))]
  ))


(define (adds-to-env fargs args env)
  (if (= (length fargs) 0)
      env
      (adds-to-env (cdr fargs) (cdr args) (add-to-env (first fargs) (first args) env))))


(define (add-to-env id val env)
  (match env
    [mtv-env (cons-env id val mtv-env)]
    [(cons-env a b renv) (cons-env a b (add-to-env id val renv))]))