#lang racket/base
(require (for-syntax racket/base "methods.rkt"))

(define-syntax (define-methods stx)
  (syntax-case stx ()
    [(_ id)
     #`(define id '#,(methods))]))

(define-methods table)

(provide table)