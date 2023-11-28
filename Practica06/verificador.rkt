#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; TCFWSBAE x TypeContext -> Type
(define (typeof expr ctx)
  (type-case TCFWSBAE expr
    [num (n) (numberT)]
    [bool (b) (booleanT)]
    [id (i) (error 'i)]
    [op (f args) (cond
                   [(equal? f +) (if (andmap (lambda (x) (equal? (typeof x ctx)
                                                                 (numberT)))args)
                                     (numberT)
                                     (error 'typeof))])]
    [iF (c t e) (if (equal? (typeof c ctx) (booleanT))
                    (if (equal? (typeof t ctx) (typeof e ctx))
                        (typeof t ctx)
                        (error 'typeof))
                    (error 'typeof))]
    ;[with (bs bo)]
    [else (error 'o)]))


