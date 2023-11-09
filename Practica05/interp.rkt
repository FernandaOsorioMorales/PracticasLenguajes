#lang plai

(require "grammars.rkt")
(require "parser.rkt")

(define (boxed-RCFSBAE-Val? b)
  (and (box? b) (RCFSBAE-Val? (unbox b))))

(define-type RCFSBAE-Val
  [num-v (n number?)]
  [bool-v (b boolean?)]
  [string-v (s string?)]
  [closure-v (args (listof symbol?)) (body RCFSBAE?) (env Env?)])

(define-type Env
  [mt-env]
  [cons-env (id symbol?) (value RCFSBAE-Val?) (rest-env Env?)]
  [rec-cons-env (id symbol?) (value boxed-RCFSBAE-Val?) (rest-env Env?)])

;; RCFSBAE x Env -> RCFSBAE-Val
  (define (interp expr env)
    (match expr
      [(numS n) (num-v n)]
      [(boolS b) (bool-v b)]
      [(strinGS s) (string-v s)]
      [(idS x) (lookup x env)]
      [(funS param body) (closure-v param body env)]
      [(opS f args) (interp (apply f (map (lambda (x) (interp x)) args)) mt-env)]
      [(appS f args)
         (let ([fun-val (interp f env)])
           (interp (closure-v-body fun-val)  (get-env (closure-v-args fun-val) args (closure-v-env fun-val))))]
      [(iFS test then other) (interp-if (interp test env) then other env)]
      [(rec bin bo)
        (interp bo
                (cyclically-bind-and-interp bo bin env))] ))

#|(define (rec-to-list-rec bin bo)
  (cond
    [(empty? bin) bo]
    [else ((list(car bin)) (rec-to-list-rec (cdr bin) bo))]))|#

(define (get-env vars args env)
  (if (= (length vars) (length args))
      (add-to-env vars args env)
      (error 'interp "Numero de argumentos y parametros distinto")))

(define (add-to-env vars args env)
  (if (empty? vars)
     env
     (cons-env (car vars) (interp (car args) env) (add-to-env (cdr vars) (cdr args) env))))


(define (interp-if test then other env)
  (type-case RCFSBAE-Val test
    [bool-v (v) (if v (interp then env) (interp other env))]
    [else (error 'interp "interp: La condición de una expresión if debe ser un booleano.")]))


(define (cyclically-bind-and-interp bound-id named-expr env)
  (local ([define value-holder (box (num-v 1729))]
          [define new-env (rec-cons-env bound-id value-holder env)]
          [define named-expr-val (interp named-expr new-env)])
    (begin
      (set-box! value-holder named-expr-val)
      new-env)))
  

;; symbol x Env -> RCFSBAE-Val
(define (lookup sub-id env)
  (type-case Env env
    [mt-env() (error 'interp "Variable libre foo")]
    [cons-env (bound-name bound-value rest-env)
              (if (symbol=? bound-name sub-id)
                  bound-value
                  (lookup sub-id rest-env))]
    [rec-cons-env (bound-name boxed-bound-value rest-env)
             (if (symbol=? bound-name sub-id)
                 (unbox boxed-bound-value)
                 (lookup sub-id rest-env))]))
