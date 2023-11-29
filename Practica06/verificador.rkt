#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; TCFWSBAE x TypeContext -> Type
(define (typeof expr ctx)
  (type-case TCFWSBAE expr
     [id (i)(lookup i ctx)]
     [num (n) (numberT)]
     [bool (b) (booleanT)]
     [strinG (s) (stringT)]
     [fun (params return-type body)(funT (list-types params (verify-type body return-type (create-context params ctx))))]
     [op (f args)
        (for ([arg (in-list args)])
                (verify-type arg (get-type-params f) ctx))
                 (get-type-returns f)]
     [app (fun-expr arguments)
         (let ([fun-type (typeof fun-expr ctx)])
                 (if (verify-args (funT-params fun-type) arguments ctx)
                     (last (funT-params fun-type))
                        (error 'typeof "Parametros mal")))]
     [iF (test-expr then-expr else-expr)
           (if (equal? (typeof test-expr ctx) (booleanT))
               (if (equal? (typeof then-expr ctx) (typeof else-expr ctx))
                   (typeof then-expr ctx) (error 'typeof "El tipo de la rama else tiene que ser el mismo que el de las ramas then"))
                     (error 'typeof (format "El tipo de las condicionales tiene que ser boolean. Tipo dado: ~v" (typeof test-expr ctx))))]
     [conD (conditions else-expr)
          (let* ([type (typeof else-expr ctx)]) (check-conditions conditions type ctx))]
     [with  (bs bo) (correct-bs bs ctx) (typeof bo (create-context (bs-to-params bs) ctx))]
     [with* (bs bo) (typeof (with*-to-with bs bo) ctx)]))


;; Funciones Op
(define (get-type-params f)
  (cond
    [(oR (equal? f +) (equal? f -) (equal? f *) (equal? f /) (equal? f min) (equal? f max) (equal? f sqrt) (equal? f <) (equal? f >) (equal? f =)
         (equal? f modulo) (equal? f expt) (equal? f add1) (equal? f sub1)(equal? f number?) (equal? f zero?))(numberT)]
    [(oR (equal? f not) (equal? f boolean?) (equal? f oR) (equal? f anD)) (booleanT)]
    [(oR (equal? f 'string?) (equal? f string-length)) (stringT)]
    [else (error 'typeof "Funcion no soportada")]))

(define (get-type-returns f)
  (cond
    [(oR (equal? f +) (equal? f -) (equal? f *) (equal? f /) (equal? f min)
         (equal? f max) (equal? f sqrt) (equal? f string-length)
         (equal? f modulo) (equal? f expt) (equal? f add1) (equal? f sub1)
         )
     (numberT)]
    [(oR (equal? f not) (equal? f boolean?) (equal? f oR) (equal? f anD) (equal? f 'string?)
         (equal? f <) (equal? f >) (equal? f =) (equal? f number?) (equal? f zero?))
     (booleanT)]
    [else (error 'typeof "Funcion no soportada")]))


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
(define (with*-to-with bs bo)
  (cond
    [(empty? bs) bo]
    [else (with (list (car bs)) (with*-to-with (cdr bs) bo))]))


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
      (cons (param-type (car params)) (list-types (cdr params) return-type))))

(define (lookup sub-id context)
  (type-case TypeContext context
             [phi () (error 'typeof "El identificador no se encuentra en el contexto.")]
             [gamma (id type rst)
                    (if (symbol=? id sub-id)
                        type
                        (lookup sub-id rst))]))

(define (tostr-funt params)
  (if (equal? (length params) 1)
      (format "~v" (car params))
      (string-append (format "~v " (car params)) (tostr-funt (cdr params)))))


