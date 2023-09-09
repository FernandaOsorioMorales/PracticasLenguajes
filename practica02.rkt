#lang plai

(define (any? x)
  #t)

(define-type Punto
  [punto (x number?) (y number?)]
  )

;; Ejercicio 1.a)
(define (punto-medio p q)
  (error 'punto-medio "Sin implementar"))

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
(define (longitud ls)
  (error 'longitud "Sin implementar"))

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
  (error 'intercala "Sin implementar"))

;; Ejercicio 2.d)
(define (aplana ls)
  (error 'aplana "Sin implementar"))

;;Definición de un Árbol Binario de Búsqueda
(define-type ArbolBinarioDeBusqueda
    [ArbolVacio]
    [ABB (elemento number?)
         (izq ArbolBinarioDeBusqueda?)
         (der ArbolBinarioDeBusqueda?)])

;; Ejercicio 3.a)
(define (elimina e a)
  (error 'elimina "Sin implementar"))

;;Función auxiliar para pasar un árbol a una lista
(define (arbolALista ab)
   (if (empty? ab)
     empty
     (append (arbolALista (node-right ab))
             (cons (node-val ab)
                   (arbolALista (node-left ab))))))
;; Ejercicio 3.b)
#|Recibe un árbol binario de búsqueda y una función, aplica esta
última a cada elemento del árbol y devuelve el árbol resultado de esto.|#
(define (mapea-arbol ab f)
  (error 'map "Sin implementar")
  )


;; Ejercicio 3.c)
(define (hojas ar)
  (error 'hojas "Sin implementar"))

;; Punto Extra
(define (mas-repetido ls)
  (error 'mas-repetido "Sin implementar"))
