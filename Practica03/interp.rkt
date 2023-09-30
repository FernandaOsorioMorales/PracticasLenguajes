   #lang plai

(require "grammars.rkt")
(require "parser.rkt")

; Función interp que recibe la expresión del parse y se encarga de evaluarla.
(define (interp expr)
  (type-case WAE expr
    [id (i) (error 'interp (format "Variable libre: '~a" (id-i expr)))]
    [num (n) n]
    [bool (b) b]
    [strinG (s) s]
    [op(f args) (apply f (map (lambda (x) (interp x)) args))]
    [with (assings body)
                   (interp (subst-lista-bindings (interpABindings assings) body))]
    [with* (assigns body) (interp (toWith assigns body))]))

; Función subst que recibe un id, un valor y una expresión y se encarga de sustiuir la variable con el valor en la
; expresión 
(define (subst  sub-id val expr)
  (type-case WAE expr
    [id (i) (if (symbol=? i sub-id) val expr)]
    [num (n) expr]
    [bool (b) (bool b)]
    [strinG (s) expr]
    [op (f args) (op f (map (lambda (x) (subst sub-id val x)) args))]
    [with (bindings body) (subst-with (with bindings body) sub-id val)]
    [with* (bindings body) (subst sub-id val (toWith bindings body))]))


; Función  auxiliar que nos ayuda a hacer subst con el with 
(define (subst-with elwith sub-id val)
  (if (isBindingIn? sub-id (with-ass elwith))
      (with (substBindings sub-id val (with-ass elwith)) (get-with-body elwith))
      (with (substBindings sub-id val (with-ass elwith)) (subst sub-id val (get-with-body elwith)))))

; Función auxilizar que revisa si un binding se encuentra en una expresión.
(define (isBindingIn? id a)
  (match a ['() #f]
    [(cons x xs) (if (symbol=? (binding-id x) id) #t (isBindingIn? id xs))]))


(define (with-ass w)
  (match w
    [(with ws wb) ws]
    [(with* ws wb) ws]))

;Función auxiliar que obtiene el cuerpo del with
(define (get-with-body w)
  (match w
    [(with ws wb) wb]
    [(with* ws wb) wb]))

; Función auxiliar que sustituye en una lista de bindings. 
(define (substBindings sub-id val bs)
  (match bs
    ['() empty]
    [(cons x xs) (cons (binding (get-binding-id x) (subst sub-id val (get-bindning-value x))) (substBindings sub-id val xs))]))


(define (get-bindning-value x)
  (match x
    [(binding id val) val]))

(define (get-binding-id x) (match x [(binding id val) id]))


; Función para pasar de un with* a un with normal
(define (toWith bindings body)
 (cond
    [(empty? bindings) body]
    [else (with (list (car bindings)) (toWith (cdr bindings) body))]))

; Subst a la lista de bindings
(define (subst-lista-bindings bindings body)
  (cond
    [(empty? bindings) body]
    [else
     (subst
      (binding-id (car bindings))
      (binding-value (car bindings))
      (subst-lista-bindings (cdr bindings) body))]))
; 

; Interp a la lista de bindings
(define (interpABindings bindings)
  (cond
    [(empty? bindings) '()]
    [else
     (cons
      (type-case Binding (car bindings)
        [binding (id value) (binding id (parse (interp value)))])
      (interpABindings (cdr bindings)))]))
