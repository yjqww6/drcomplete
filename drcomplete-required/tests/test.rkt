#lang racket
(require "../private/walk.rkt")
(module a racket/base
  (define ftest #f)
  (provide ftest))

(module+ test
  (require (for-syntax syntax/parse) rackunit)

  (define (get-imports module-form prologue)
    (define ns (make-base-namespace))
    (parameterize ([current-namespace ns])
      (when prologue
        (prologue))
      (define fpe (expand module-form))
      (walk-module fpe)))

  (define-check (check-in module-form proc prologue)
    (unless (proc (get-imports module-form prologue))
      (fail-check)))

  (define-syntax (check-member stx)
    (syntax-parse stx
      [(_ module-form (~optional (~seq #:not (e ...))
                                 #:defaults ([(e 1) '()]))
          (~optional (~seq #:prologue prologue)
                     #:defaults ([prologue #'#f]))
          id ...)
       (syntax/loc stx
         (check-in module-form
                   (λ (imports)
                     (and
                      (set=?
                       (set-intersect (set 'e ...) imports)
                       (set))
                      (subset? (set 'id ...) imports)))
                   prologue))]))

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
                   (require (submod "test.rkt" a)))
                walk-module tool@ ftest)


  (check-member '(module a racket/base
                   (require (prefix-in g: racket/gui)
                            (rename-in racket/control
                                       [call/prompt callp])
                            (except-in racket/exn exn->string)))
                #:not (exn->string) g:frame% callp)

  (check-member '(module a racket
                   (define-syntax-rule (test1)
                     (require (rename-in racket/base [call/cc callcc1])))
                   (define-syntax (test2 stx)
                     #`(require (rename-in racket/base [call/cc #,(syntax-local-introduce #'callcc2)])))
                   (test1)
                   (test2))
                #:not (callcc1) callcc2)

  (check-member '(module a racket/base
                   (require (for-syntax racket/base))

                   (define-syntax (test1 stx)
                     #`(#%require (prefix p1: racket/set)
                                  (only racket/set set?)))

                   (define-syntax (test2 stx)
                     (define mod (syntax-local-introduce #'racket/set))
                     #`(#%require (prefix p2: #,mod)
                                  (only #,mod set->list)))

                   (test1)
                   (test2))
                #:not (p1:set? set?) p2:set? set->list)
  (check-member '(module a racket
                   (define module 1)

                   (begin-for-syntax
                     (module a racket/base
                       (require racket/syntax))))
                #:not () format-id)
  (check-member '(module a racket
                   (begin-for-syntax
                     (require racket/syntax)))
                #:not () format-id)
  (check-member '(module a racket
                   (begin-for-syntax
                     (module* a #f
                       (require racket/syntax))))
                #:not () format-id)

  (check-member '(module a racket
                   (#%require (just-meta #f "b.rkt")))
                #:prologue
                (λ ()
                  (parameterize ([current-module-declare-name
                                  (make-resolved-module-path
                                   (build-path (current-directory) "b.rkt"))])
                    (eval
                     '(module b racket/base
                        (require (for-label syntax/stx))
                        (provide (for-label stx-pair?))))))
                stx-pair?)
  )