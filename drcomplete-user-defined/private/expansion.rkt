#lang racket/base

(require "main.rkt" racket/set racket/exn)
(provide go)
(define (go v path src cust)
  (cond
    [(exn? v) #f]
    [else
     (with-handlers ([exn?
                      (λ (e)
                        (log-message
                         (current-logger) 'error 'drcomplete
                         (exn->string e) (current-continuation-marks))
                        #f)])
       (for/list ([s (in-set (walk v))])
         (symbol->string s)))]))