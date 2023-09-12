#lang plai

(define (any? x)
  #t)

(define-type Punto
  [punto (x number?) (y number?)]
  )

;; Ejercicio 1.a)
;punto-medio :: Punto Punto -> Punto
;Funcion que recibe dos puntos y regresa el punto medio
;entre ellos
(define (punto-medio p q)
  (if (and (Punto? p) (Punto? q))
      [punto  (/ (+ (punto-x p)(punto-x q)) 2)  (/ (+ (punto-y p)(punto-y q)) 2)]
      "No se puede realizar la operacion, al menos uno no es punto"))

;Ejercicio 1.b
;Función que recibe dos puntos y devuelve la distancia entre ellos.
;distancia ::Punto Punto -> Number

(define (distancia p q)
    (if(and(Punto? p)(Punto? q))
  (sqrt(+(sqr(-(punto-y p)(punto-x p)))
         (sqr(-(punto-y q)(punto-x q)))))
      "No se puede porque al menos uno no es un punto")
 )

(define-type Lista
    [Vacia]
    [Cons (cabeza any?) (resto Lista?)])

;; Ejercicio 2.a)
;longitud :: Lista -> number
;Funcion que calcula la longitud de una lista
;Pasar la lista de esta manera : (longitud '(1 2 3 4))
(define (longitud ls)
  (if (empty? ls)
      0
      (+ 1 (longitud (cdr ls)))))


;; Ejercicio 2.b)
#| Predicado pertenece el cual recibe un elemento y una lista y devuelve #t
si dicho elemento se encuentra en la lista y #f en cualquier otro caso|#
(define (pertenece? e ls)
  (cond
    [(empty? ls) false ]
    [(=(first ls)e) true]
    [else (pertenece? e (rest ls))]
   )
  )

;; Ejercicio 2.c)
(define (intercala ls ks)
  (cond
    [(empty? ls) ks]
    [(empty? ks) ls]
    [else (cons (car ls) (cons (car ks) (intercala (cdr ls) (cdr ks))))]
  ))

;; Ejercicio 2.d)
(define (aplana ls) 
  (cond 
    [(empty? ls) ls]
    [(pair? ls) (append (aplana (car ls)) (aplana (cdr ls)))]
    [else (list ls)]))


;;Definición de un Árbol Binario de Búsqueda
(define-type ArbolBinarioDeBusqueda
    [ArbolVacio]
    [ABB (elemento number?)
         (izq ArbolBinarioDeBusqueda?)
         (der ArbolBinarioDeBusqueda?)])

;; Ejercicio 3.a)
;elimina :: ArbolBinarioDeBusqueda number -> ArbolBinarioDeBusqueda
;Funcion que toma un árbol binario de búsqueda y un elemento, elimina este último
;del árbol y regresa el árbol resultante de dicha operación.
(define (elimina arbol e)
  (type-case ArbolBinarioDeBusqueda arbol
    [ArbolVacio () (ArbolVacio)]
    [ABB (el izq der)
         (cond
           [(= e el)
            (cond
              [(ArbolVacio? izq) der]
              [(ArbolVacio? der) izq]
              [else
               (ABB (raizNueva izq)(elimina izq (raizNueva izq)) der)])]
           [(> e el) (ABB el (elimina izq e) der) ]
           [(< e el) (ABB el izq(elimina der e))])]))

;Funcion Auxiliar
;raizNueva :: ArbolBinarioDeBusqueda -> number
;Funcion que calcula la nueva raiz de un arbol binario
(define (raizNueva arbol)
  (type-case ArbolBinarioDeBusqueda arbol
    [ArbolVacio () (ArbolVacio)]
    [ABB (elemento izq der)
         (if (ArbolVacio? der)
             elemento
             (raizNueva der))]))



;; Ejercicio 3.b)
#|Recibe un árbol binario de búsqueda y una función, aplica esta
última a cada elemento del árbol y devuelve el árbol resultado de esto.|#
(define (mapea-arbol ab f)
  (if (ArbolVacio? ab)
      (ArbolVacio)
      (ABB(f(ABB-elemento ab))
               (mapea-arbol (ABB-izq ab)f)
               (mapea-arbol (ABB-der ab)f))))


;; Ejercicio 3.c)
(define (hojas ar)
  (type-case ArbolBinarioDeBusqueda ar
    [ArbolVacio () '()]
    [ABB (elemento izq der)
         (if (and (ArbolVacio? izq) (ArbolVacio? der))
             (list elemento)
             (append (hojas izq) (hojas der)))]))

;; Punto Extra
(define (mas-repetido ls)
  (error 'mas-repetido "Sin implementar"))

