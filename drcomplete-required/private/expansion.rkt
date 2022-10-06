#lang racket/base

(require "utils.rkt" racket/set racket/exn)
(provide go)
(cond-use-bound
 (require "walk-bound.rkt")
 #:else
 (require "walk.rkt"))
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
       (for/list ([s (in-set (walk-module v))])
         (symbol->string s)))]))
