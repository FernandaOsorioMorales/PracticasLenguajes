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
     [fun (params tipo bo)(funT (list-types params (verify-type bo tipo (create-context params ctx))))]
     [op    (f args)
        (for ([arg (in-list args)])
                (verify-type arg (obtener-tipo f) ctx))
                 (obtener-tipo-r f)]
     [app (fun-expr arguments)
         (let ([fun-type (typeof fun-expr ctx)])
                 (if (verify-args (funT-params fun-type) arguments ctx)
                     (last (funT-params fun-type))
                        (error 'typeof "Error en los parametros")))]
     [iF (test-expr then-expr else-expr)
           (if (equal? (typeof test-expr ctx) (booleanT))
               (if (equal? (typeof then-expr ctx) (typeof else-expr ctx))
                   (typeof then-expr ctx) (error 'typeof "El tipo de la rama else tiene que ser el mismo que el de las ramas then"))
                     (error 'typeof (format "El tipo de las condicionales tiene que ser boolean. Tipo dado: ~a" (typeof test-expr ctx))))]
     [conD (conditions else-expr)
          (let* ([type (typeof else-expr ctx)]) (check-conditions conditions type ctx))]
     [with  (bs bo) (correct-bs bs ctx) (typeof bo (create-context (bs-to-params bs) ctx))]
     [with* (bs bo) (typeof (withEstrella-to-with bs bo) ctx)]))


;; Funciones Op

(define (obtener-tipo f)
  (cond
    [(oR (equal? f +) (equal? f -) (equal? f *) (equal? f /) (equal? f min) (equal? f max)
         (equal? f sqrt) (equal? f <) (equal? f >) (equal? f =) (equal? f modulo) 
         (equal? f expt) (equal? f add1) (equal? f sub1) (equal? f number?) 
         (equal? f zero?))(numberT)]
    [(oR (equal? f not) (equal? f boolean?) (equal? f oR) (equal? f anD))
     (booleanT)]
    [(oR (equal? f 'string?) (equal? f string-length))(stringT)]
    [else (error 'typeof "Funcion no permitida en el lenguaje")]))



(define (obtener-tipo-r f)
  (cond
    [(oR (equal? f +) (equal? f -) (equal? f *) (equal? f /) (equal? f min)
         (equal? f max) (equal? f sqrt) (equal? f string-length)
         (equal? f modulo) (equal? f expt) (equal? f add1) (equal? f sub1)
         )
     (numberT)]
    [(oR (equal? f not) (equal? f boolean?) (equal? f oR) (equal? f anD) (equal? f 'string?)
         (equal? f <) (equal? f >) (equal? f =) (equal? f number?) (equal? f zero?))
     (booleanT)]
    [else (error 'typeof "Funcion no peritida en el lenguaje")]))


;;Revision Tipos
(define (verify-args params args ctx)
  (if (empty? args)
      #t
      (if (equal? (car params) (typeof (car args) ctx))
          (verify-args (cdr params) (cdr args) ctx)
          (error 'typeof (format "El tipo de los parametros no es el esperado. Tipo dado: ~v. Tipo esperado: ~v" (typeof (car args) ctx) (car params))))))

(define (check-conditions conditions type ctx)
  (if (empty? conditions)
      type
      (if (equal? (typeof (condition-test-expr (car conditions)) ctx) (booleanT))
          (if (equal? (typeof (condition-then-expr (car conditions)) ctx) type)
              (check-conditions (cdr conditions) type ctx)
              (error 'typeof "El tipo de la rama else tiene que ser el mismo que el de las ramas then"))
          (error 'typeof "Condicion booleana"))))

(define (verify-type body return-type context)
  (if (equal? (typeof body context) return-type)
      return-type
      (if (funT? return-type)
          (error 'typeof (format "El tipo de retorno de la función no coincide con el tipo de su cuerpo. Tipo dado: ~v. Tipo esperado: (funT (~a))." (typeof body context) (tostr-funt (funT-params return-type))))
          (error 'typeof (format "Error en el parametro ~v. Tipo esperado: ~v. Tipo dado: ~v" body return-type (typeof body context))))))

;; Funciones with
(define (bs-to-params bs)
  (if (empty? bs)
      '()
      (cons (param (binding-id (car bs)) (binding-type (car bs)))
                   (bs-to-params (cdr bs)))))
(define (correct-bs bs ctx)
  (if (empty? bs)
      #t
      (if (equal? (typeof (binding-value (car bs)) ctx) (binding-type (car bs)))
          (correct-bs (cdr bs) ctx)
          (error 'typeof (format "El tipo de la asignación no corresponde con el del valor. Tipo esperado: ~v. Tipo dado: ~v." (binding-type (car bs)) (typeof (binding-value (car bs)) ctx) )))))          
      
;; with*-to-with :: binding -> WAE -> WAE
;; Pasa un with* a with usando sintactic sugar
(define (withEstrella-to-with bs bo)
  (cond
    [(empty? bs) bo]
    [else (with (list (first bs)) (withEstrella-to-with (rest bs) bo))]))


;; Se crea contexto
(define (create-context params context)
  (if (empty? params)
      context
      (gamma
       (param-id (car params))
       (param-type (car params))
       (create-context (cdr params) context))))

;; Otras
(define (list-types params return-type)
  (if (empty? params)
      (cons return-type '())
      (cons (param-type (first params)) (list-types (rest params) return-type))))

(define (lookup sub-id ctx)
  (type-case TypeContext ctx
             [phi () (error 'typeof "Variable libre.")]
             [gamma (id type rst)
                    (if (symbol=? id sub-id)
                        type
                        (lookup sub-id rst))]))

(define (tostr-funt params)
  (if (equal? (length params) 1)
      (format "~a" (car params))
      (string-append (format "~a " (car params)) (tostr-funt (cdr params)))))


