#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; TCFWSBAE x TypeContext -> Type
(define (typeof expr ctx)
  (type-case TCFWSBAE expr
     [num (n) (numberT)]
     [bool (b) (booleanT)]
     [strinG (s) (stringT)]
     [id (i)(lookup i ctx)]
     [fun (params tipo bo)(funT (tipo-lista params (revisa-tipo bo tipo (crear-ctx params ctx))))]
     [op    (f args)
        (for ([arg (in-list args)])
                (revisa-tipo arg (obtener-tipo f) ctx))
                 (obtener-tipo-r f)]
     [app (f-expr args)
         (let ([fun-type (typeof f-expr ctx)])
                 (if (revisa-args (funT-params fun-type) args ctx)
                     (last (funT-params fun-type))
                        (error 'typeof "Error en los parametros")))]
     [iF (con thn elsE)
           (if (equal? (typeof con ctx) (booleanT))
               (if (equal? (typeof thn ctx) (typeof elsE ctx))
                   (typeof thn ctx) (error 'typeof "El tipo de la rama else tiene que ser el mismo que el de las ramas then"))
                     (error 'typeof (format "El tipo de las condicionales tiene que ser boolean. Tipo dado: ~v" (typeof con ctx))))]
     [conD (con elsE)
          (let* ([type (typeof elsE ctx)]) (revisa-cond con type ctx))]
     [with  (bs bo) (verifica-bindings bs ctx) (typeof bo (crear-ctx (transforma-bindings-to-p bs) ctx))]
     [with* (bs bo) (typeof (withEstrella-to-with bs bo) ctx)]))


;; Funciones auxiliares para Operaciones

(define (obtener-tipo f)
  (cond
    [(oR (equal? f +) (equal? f -) (equal? f *) (equal? f /) (equal? f max) (equal? f min)
         (equal? f expt) (equal? f <) (equal? f >) (equal? f =) (equal? f modulo) 
         (equal? f sqrt) (equal? f add1) (equal? f sub1) (equal? f zero?) 
         (equal? f number?))(numberT)]
    [(oR (equal? f not) (equal? f boolean?) (equal? f oR) (equal? f anD))
     (booleanT)]
    [(oR (equal? f 'string?) (equal? f string-length))(stringT)]
    [else (error 'typeof "Funcion no permitida en el lenguaje")]))



(define (obtener-tipo-r f)
  (cond
    [(oR (equal? f +) (equal? f -) (equal? f *) (equal? f /) (equal? f max)
         (equal? f min) (equal? f expt) (equal? f string-length)
         (equal? f add1) (equal? f sqrt) (equal? f sub1) (equal? f modulo)
         )
     (numberT)]
    [(oR (equal? f anD) (equal? f oR) (equal? f not) (equal? f boolean?) (equal? f 'string?)
         (equal? f >) (equal? f <) (equal? f =) (equal? f number?) (equal? f zero?))
     (booleanT)]
    [else (error 'typeof "Funcion no permitida en el lenguaje")]))


;;Funciones para revisar tipos
(define (revisa-args params args ctx)
  (if (empty? args)
      #t
      (if (equal? (first params) (typeof (first args) ctx))
          (revisa-args (rest params) (rest args) ctx)
          (error 'typeof (format "El tipo de los parametros no es el esperado. Tipo dado: ~v. Tipo esperado: ~v" (typeof (first args) ctx) (first params))))))

(define (revisa-cond con tipo ctx)
  (if (empty? con)
      tipo
      (if (equal? (typeof (condition-test-expr (car con)) ctx) (booleanT))
          (if (equal? (typeof (condition-then-expr (car con)) ctx) tipo)
              (revisa-cond (cdr con) tipo ctx)
              (error 'typeof "El tipo de la rama else tiene que ser el mismo que el de las ramas then"))
          (error 'typeof "Condicion booleana"))))

(define (revisa-tipo bo tipo-r ctx)
  (cond
    [(equal? (typeof bo ctx) tipo-r) tipo-r]
    [(funT? tipo-r)
         (error 'typeof (format "El tipo de retorno de la función no coincide con el tipo de su cuerpo. Tipo dado: ~v. Tipo esperado: (funT (~v))." (typeof bo ctx) (to-str (funT-params tipo-r))))]
    [else
         (error 'typeof (format "Error en el parametro ~v. Tipo esperado: ~v. Tipo dado: ~v" bo tipo-r (typeof bo ctx)))]))


;; Funciones auxiliares  with
(define (transforma-bindings-to-p bs)
  (cond
    [(empty? bs) empty]
    [else (cons (param (binding-id (first bs)) (binding-type (first bs))) (transforma-bindings-to-p (rest bs)))]))

(define (verifica-bindings bs ctx)
  (cond
    [(empty? bs) #t]
    [(equal? (typeof (binding-value (car bs)) ctx) (binding-type (car bs)))
         (verifica-bindings (cdr bs) ctx)]
    [else
         (error 'typeof (format "El tipo de la asignación no corresponde con el del valor. Tipo esperado: ~v. Tipo dado: ~v."
                                (binding-type (car bs)) (typeof (binding-value (car bs)) ctx)))]))
          
     
;; Pasa un with estrella a with normal
(define (withEstrella-to-with bs bo)
  (cond
    [(empty? bs) bo]
    [else (with (cons (first bs)) (withEstrella-to-with (rest bs) bo))]))


;; Creamos contexto 
(define (crear-ctx params context)
  (if (empty? params)
      context
      (gamma
       (param-id (car params))
       (param-type (car params))
       (crear-ctx (cdr params) context))))

; Funciones auxiliares extras
(define (tipo-lista parametros tipo-r)
  (cond
    [(empty? parametros) (cons tipo-r '())]
    [else (cons (param-type (first parametros)) (tipo-lista (rest parametros) tipo-r))]))


(define (lookup sub-id ctx)
  (type-case TypeContext ctx
             [phi () (error 'typeof "Variable libre.")]
             [gamma (id type rst)
                    (if (symbol=? id sub-id)
                        type
                        (lookup sub-id rst))]))

(define (to-str parametros)
  (cond
    [(equal? (length parametros) 1) (format "~v" (car parametros))]
    [else (string-append (format "~v " (car parametros)) (to-str (cdr parametros)))]))



