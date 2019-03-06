#lang racket
(require "../private/main.rkt")

(module+ test
  (require rackunit)

  (define ns (make-base-namespace))
  
  (define-check (check-in module-form expected)
    (define fpe (parameterize ([current-namespace ns])
                  (expand module-form)))
    (define actual (walk fpe))
    (with-check-info
        (['expected expected]
         ['actual actual])
      (unless (set=? expected actual)
        (fail-check))))

  (define-syntax-rule (check-same module-form id ...)
    (check-in module-form (seteq 'id ...)))

  (check-same '(module a racket/base
                 (define f 0)
                 (struct A (a b c)))
              f A A-a A-b A-c struct:A A?)

  (check-same '(module a racket/base
                 (define-syntax f (syntax-rules ())))
              f)

  (check-same '(module a racket/base
                 (lambda (a [b #f] #:c [c #f] . d)
                   (let ([f call/cc])
                     (f call/cc))))
              a b c d f)
  
  (check-same '(module a racket/base
                 (require (for-syntax racket/base
                                      (for-syntax racket/base)))
                 (begin-for-syntax
                   (begin-for-syntax
                     (let ([f call/cc])
                       (f call/cc)))))
              f)

  (check-same '(module a racket/base
                 (let ()
                   (define f 1)
                   (define-syntax a (syntax-rules ()))
                   f))
              f a)

  (check-same '(module a racket/base
                 (require (for-syntax racket/base))
                 (define-syntax (a stx)
                   (define f #f)
                   (define-syntax b (syntax-rules ()))
                   stx))
              stx f a b)

  (check-same '(module a racket/base
                 (require (for-syntax racket/base))
                 (let ()
                   (define-syntax (a stx)
                     ;really disappeared bindings
                     (define f #f)
                     (define-syntax b (syntax-rules ()))
                     stx)
                   (void)))
              a)

  (check-same '(module a racket/base
                 (require (for-syntax racket/base
                                      (for-syntax racket/base)))
                 (begin-for-syntax
                   (begin-for-syntax
                     (module b racket/base
                       (module c racket/base
                         (let ()
                           (define (f x) x)
                           (define-syntax a (syntax-rules ()))
                           (f (λ (y) y))))))))
              f a x y)
  )