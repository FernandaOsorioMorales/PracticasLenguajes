#lang plai

(require "grammars.rkt")
(require "parser.rkt")

(define (boxed-RCFSBAE-Val? b)
  (and (box? b) (RCFSBAE-Val? (unbox b))))

;Definición del nuevo valor
(define-type RCFSBAE-Val
  [num-v (n number?)]
  [bool-v (b boolean?)]
  [string-v (s string?)]
  [closure-v (args (listof symbol?)) (body RCFSBAE?) (env Env?)])

;Definición del ambiente
(define-type Env
  [mt-env]
  [cons-env (id symbol?) (value RCFSBAE-Val?) (rest-env Env?)]
  [rec-cons-env (id symbol?) (value boxed-RCFSBAE-Val?) (rest-env Env?)])

;; RCFSBAE x Env -> RCFSBAE-Val
(define (interp expr env)
   (match expr
      [(num n) (num-v n)]
      [(bool b) (bool-v b)]
      [(strinG s) (string-v s)]
      [(id x) (lookup x env)]
      [(fun param body) (closure-v param body env)]
      [(op f args) (interp (parse (apply f (value-list (map (lambda (e) (interp e env)) args)))) mt-env)]
      [(app f args)
         (let ([fun-val (interp f env)])
           (interp (closure-v-body fun-val)  (get-env (closure-v-args fun-val) args (closure-v-env fun-val))))]
      [(iF test then other) (interp-if (interp test env) then other env)]
     [(rec bindings body)
                  (let* ([mt (nuevo-ambienteRec bindings env)]
                         )
                    (interp body mt))]
))

;Función auxiliar de interp con rec
(define (nuevo-ambienteRec bindings env)
  (if (empty? bindings)
      env
      (type-case RCFSBAE (binding-value (car bindings))
        [fun (parameters body)(nuevo-ambienteRec (cdr bindings)(cyclically-bind-and-interp
             (binding-id (car bindings))(binding-value (car bindings)) env))]
        [else (nuevo-ambienteRec (cdr bindings) (cons-env (binding-id (car bindings))
                 (interp (binding-value (car bindings)) env)env))]
                 )))

;Función cyclically-bind-and-interp
(define (cyclically-bind-and-interp bound-id named-expr env)
  (local ([define value-holder (box (num-v 1729))]
          [define new-env (rec-cons-env bound-id value-holder env)]
          [define named-expr-val (interp named-expr new-env)])
    (begin
      (set-box! value-holder named-expr-val)
      new-env)))

;Función auxiliar de interp aplicación de función
(define (get-env vars args env)
  (if (= (length vars) (length args))
      (add-to-env vars args env)
      (error 'interp "Numero de argumentos y parametros distinto")))

;Función auxiliar de get-env
(define (add-to-env vars args env)
  (if (empty? vars)
     env
     (cons-env (car vars) (interp (car args) env) (add-to-env (cdr vars) (cdr args) env))))

;Función auxiliar de interp con op
(define (value-list args)
  (if (empty? args) empty
      (type-case RCFSBAE-Val (car args)
        [num-v (n) (cons n (value-list (cdr args)))]
        [bool-v (b) (cons b (value-list (cdr args)))]
        [string-v (s) (cons s (value-list (cdr args)))]
        [else error 'interp "Error al obtener el valor"])))

;Función auxiliar de interp con if
(define (interp-if test then other env)
    (bool-v (test) (if test (interp then env) (interp other env))))


;; symbol x Env -> RCFSBAE-Val
;Función lookup
(define (lookup sub-id env)
  (type-case Env env
    [mt-env() (error 'interp "Variable libre ~a" sub-id)]
    [cons-env (bound-name bound-value rest-env)
              (if (symbol=? bound-name sub-id)
                  bound-value
                  (lookup sub-id rest-env))]
    [rec-cons-env (bound-name boxed-bound-value rest-env)
             (if (symbol=? bound-name sub-id)
                 (unbox boxed-bound-value)
                 (lookup sub-id rest-env))]))
