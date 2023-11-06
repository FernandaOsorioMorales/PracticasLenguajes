#lang plai

(define-type Binding
  [binding (id symbol?) (value RCFSBAE?)])


(define-type Condition
  [condition (test-expr RCFSBAE?) (then-expr RCFSBAE?)])

(define-type RCFSBAE
  [numS (n number?)]
  [idS (i symbol?)]
  [boolS (b boolean?)]
  [strinGS (s string?)]
  [opS (f procedure?) (args (listof RCFSBAE?))]
  [withS (bindings (listof Binding?)) (body RCFSBAE?)]
  [with*S (bindings (listof Binding?)) (body RCFSBAE?)]
  [funS (params (listof symbol?)) (body RCFSBAE?)]
  [appS (f RCFSBAE?) (args (listof RCFSBAE?))]
  [iFS (test-expr RCFSBAE?) (then-expr RCFSBAE?) (else-expr RCFSBAE?)]
  [conDS (conds (listof Condition?)) (else-expr RCFSBAE?)]
  [rec (bindings (listof Binding?)) (body RCFSBAE?)])