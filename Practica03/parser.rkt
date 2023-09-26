#lang plai
(require "grammars.rkt")
(define (list-to-binding ls)
  (binding (car ls) (parse (cdr ls)) ))

(define (parse s-exp)
  (cond
    [(number? s-exp) (num s-exp)]
    [(symbol? s-exp) (id s-exp)]
    [(boolean? s-exp) (bool s-exp)]
    [(list? s-exp)
     (let ([head (car s-exp)])
       (case (first s-exp)
         ;De aridad 1
         [(sub1 add1 not zero? num? str? bool? str-length )
          (if(=(length (cdr s-exp))1)
             (op (eval head (make-base-namespace))( map parse (cdr s-exp)))
             (error 'argumentos-incorrectos
                    (format "Se espera ~a argumento, y se han recibido ~a" 1 (length (cdr s-exp)))))]
         ;De aridad 2
         [(expt mod )
          (if(=(length (cdr s-exp))2)
             (op (eval head (make-base-namespace))( map parse (cdr s-exp)))
             (error 'argumentos-incorrectos
                    (format "Se esperan ~a argumentos, y se han recibido ~a" 2 (length (cdr s-exp)))))]
         ;Se espera que sean mas de 0 argumentos
         [(+ - * / min max sqrt < > <= >= = anD oR)
          (if(>(length (cdr s-exp))0)
             (op (eval head (make-base-namespace))( map parse (cdr s-exp)))
             (error 'argumentos-incorrectos
                    (format "Se esperan ~a argumentos, y se han recibido ~a" 2 (length (cdr s-exp)))))]
         ;[(with)]

                     
         [(with*) (let* ([vars (second s-exp)]
                                  [bindings (map list-to-binding
                                                 vars)])
                  (with* bindings
                         parse (third s-exp)))]))]))

