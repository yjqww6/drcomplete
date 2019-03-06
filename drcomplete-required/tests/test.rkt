#lang racket
(require (for-syntax syntax/parse) rackunit "../private/walk.rkt")

(provide check-member)

(define ns (make-base-namespace))

(define-check (check-in module-form proc)
  (define fpe (parameterize ([current-namespace ns])
                (expand module-form)))
  (define imports (walk-module fpe))
  (unless (proc imports)
    (fail-check)))

(define-syntax (check-member stx)
  (syntax-parse stx
    [(_ module-form (~optional (~seq #:not (e ...))
                               #:defaults ([(e 1) '()]))
        id ...)
     #'(check-in module-form
                 (λ (imports) (and
                               (set=?
                                (set-intersect (set 'e ...) imports)
                                (set))
                               (subset? (set 'id ...) imports))))]))

(check-member '(module a racket/base)
              call/cc)

(check-member '(module a racket/base
                 (require racket/gui))
              frame%)

(check-member '(module a racket/base
                 (module b racket/base
                   (require racket/gui)))
              frame%)

(check-member '(module a racket/base
                 (require (for-syntax racket/base)
                          (for-meta 2 racket/base))
                 (begin-for-syntax
                   (begin-for-syntax
                     (module a racket/base
                       (require racket/gui)))))
              frame%)

(check-member '(module a racket/base
                 (module a racket/base)
                 (module b racket/base
                   (require (submod ".." a))
                   (module c racket/base))
                 (require 'a (submod 'b c)))
              call/cc)

(check-member '(module a racket/base
                 (require "../tool.rkt")
                 (require "../private/walk.rkt")
                 (require "../private/expansion.rkt")
                 (require "test.rkt"))
              walk-module tool@ check-member go)


(check-member '(module a racket/base
                 (require (prefix-in g: racket/gui)
                          (rename-in racket/control
                                     [call/prompt callp])
                          (except-in racket/exn exn->string)))
              #:not (exn->string) g:frame% callp)