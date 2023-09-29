   #lang plai

(require "grammars.rkt")
(require "parser.rkt")

(define (interp expr)
  (type-case WAE expr
    [id (i) (error 'interp (format "Variable libre: ~a" (id-i expr)))]
    [num (n) n]
    [bool (b) b]
    [strinG (s) s]
    [op(f args) (apply f (map (lambda (x) (interp x)) args))]
    [with (assings body)
                   (interp (subst-lista-bindings (interpABindings assings) body))]
    [with* (assigns body) (interp (toWith assigns body))]))

(define (subst  sub-id val expr)
  (type-case WAE expr
    [id (i) (if (symbol=? i sub-id id) val expr)]
    [num (n) expr]
    [bool (b) (bool b)]
    [strinG (s) expr]
    [op (f args) (op f (map (lambda (x) (subst x sub-id val)) args))]
    [with (bindings body)
          (let ([variables (map binding-id bindings)])
                     (let ([comparador (lambda (x y) (symbol=? x y))])
                       (cond
                         [(estaVariable? sub-id variables comparador)
                          (with
                           (map (lambda (cadaPar)
                                  (binding (binding-id cadaPar) (subst sub-id val (binding-value cadaPar)))) bindings) body)]
                         [else
                          (with
                          (map (lambda (cadaPar)
                                  (binding (binding-id cadaPar) (subst sub-id val (binding-value cadaPar)))) bindings)
                          (subst sub-id val body))])))]
    
    [with* (bindings body)
           (subst sub-id val (toWith bindings body))]))



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

; Interp a la lista de bindings
(define (interpABindings bindings)
  (cond
    [(empty? bindings) '()]
    [else
     (cons
      (type-case Binding (car bindings)
        [binding (id value) (binding id (parse (interp value)))])
      (interpABindings (cdr bindings)))]))
