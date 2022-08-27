#lang racket/base
(require syntax/parse/define (for-syntax racket/base))

(define-syntax-parser cond-bound
  [(_ [(X:id ...) Body:expr ...] Rest ...)
   (if (andmap identifier-binding (syntax->list #'(X ...)))
       #'(begin Body ...)
       (syntax/loc this-syntax (cond-bound Rest ...)))]
  [(_  [(~literal else) Body:expr ...])
   #'(begin Body ...)])

(define-syntax-parse-rule (cond-use-bound Body:expr ... (~optional (~seq #:else Alt:expr ...) #:defaults ([(Alt 1) '()])))
  (cond-bound
   [(syntax-bound-phases syntax-bound-symbols)
    Body ...]
   [else Alt ...]))

(define (visible? id)
  (for/and ([scope (in-list
                    (hash-ref (syntax-debug-info id)
                              'context (λ () '())))])
    (not (eq? 'macro (vector-ref scope 1)))))

(provide (all-defined-out))