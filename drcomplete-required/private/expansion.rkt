#lang racket/base

(require "walk.rkt" racket/set racket/exn)
(provide go)
(define (go v path src cust)
  (cond
    [(exn? v) #f]
    [else
     (with-handlers ([exn? (λ (e) (displayln (exn->string e)) #f)])
       (for/list ([s (in-set (walk-module v))])
         (symbol->string s)))]))
