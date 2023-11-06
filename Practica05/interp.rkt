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
(define (interp expr env) (error 'interp "sin implementar"))

;; symbol x Env -> RCFSBAE-Val
(define (lookup sub-id env)
  (type-case Env env
    [mt-env() (error 'lookup "no binding for identifier")]
    [cons-env (bound-name bound-value rest-env)
              (if (symbol=? bound-name sub-id)
                  bound-value
                  (lookup sub-id rest-env))]
    [rec-cons-env (bound-name boxed-bound-value rest-env)
             (if (symbol=? bound-name sub-id)
                 (unbox boxed-bound-value)
                 (lookup sub-id rest-env))]))
