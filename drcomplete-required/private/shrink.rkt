#lang racket
(require (for-syntax racket/base racket/syntax) syntax/kerncase racket/stxparam)
(provide shrink-module)

(define-syntax-parameter phase-id (syntax-rules ()))

(define-syntax (define/phase stx)
  (syntax-case stx ()
    [(_ (f arg ...) body ...)
     (with-syntax ([f/p (format-id #'f "~a/phase" (syntax-e #'f))])
       #'(begin (define (f/p arg ... phase)
                  (syntax-parameterize
                      ([phase-id
                        (syntax-rules () [(_) phase])])
                    body ...))
                (define-syntax-rule (f arg ...)
                  (f/p arg ... (phase-id)))))]))

(define (shrink-module fpe)

  (define raw-specs (mutable-set))

  (define (push! sth)
    (when (ext-module-path? sth)
      (set-add! raw-specs (syntax->datum sth))))

  (define (ext-module-path? r)
    (syntax-case* r (submod quote)
      (λ (a b) (free-identifier=? a b #f #f))
      [(submod "." _ ...) #f]
      [(submod ".." _ ...) #f]
      [(submod (quote _) _ ...) #f]
      [(quote _) #f]
      [_ #t]))

  (define (phaseless-spec spec)
    (syntax-case* spec
      (only prefix all-except prefix-all-except rename)
      (λ (a b) (free-identifier=? a b #f #f))
      [(only ?raw-module-path ?id ...)
       (push! #'?raw-module-path)]
      [(prefix ?prefix-id ?raw-module-path)
       (push! #'?raw-module-path)]
      [(all-except ?raw-module-path ?id ...)
       (push! #'?raw-module-path)]
      [(prefix-all-except ?prefix-id ?raw-module-path ?id ...)
       (push! #'?raw-module-path)]
      [(rename ?raw-module-path _ _)
       (push! #'?raw-module-path)]
      [?raw-module-path
       (push! #'?raw-module-path)]))

  (define (phaseless-spec* spec*)
    (for ([spec (in-syntax spec*)])
      (phaseless-spec spec)))
    
  (define (raw-require-spec spec)
    (syntax-case* spec
      (for-meta for-syntax for-template for-label just-meta)
      (λ (a b) (free-identifier=? a b #f #f))
      [(for-meta ?level ?phaseless-spec ...)
       (phaseless-spec* #'(?phaseless-spec ...))]
      [(for-syntax ?phaseless-spec ...)
       (phaseless-spec* #'(?phaseless-spec ...))]
      [(for-template ?phaseless-spec ...)
       (phaseless-spec* #'(?phaseless-spec ...))]
      [(for-label ?phaseless-spec ...)
       (phaseless-spec* #'(?phaseless-spec ...))]
      [(just-meta ?level ?raw-require-spec ...)
       (for ([spec (in-syntax #'(?raw-require-spec ...))])
         (raw-require-spec spec))]
      [?phaseless-spec
       (phaseless-spec #'?phaseless-spec)]))

  (define/phase (walk form)
    (syntax-case* form (module module* #%require begin begin-for-syntax)
      (λ (a b) (free-identifier=? a b (phase-id) 0))
      [(module ?id ?path (#%plain-module-begin ?form ...))
       (push! #'?path)
       (walk* #'(?form ...))]
      [(module* ?id ?path (#%plain-module-begin ?form ...))
       (when (syntax-e #'?path)
         (push! #'?path))
       (walk* #'(?form ...))]
      [(#%require ?spec ...)
       (for ([spec (in-syntax #'(?spec ...))])
         (raw-require-spec spec))]
      [(begin ?form ...)
       (walk* #'(?form ...))]
      [(begin-for-syntax ?form ...)
       (walk*/phase #'(?form ...) (+ (phase-id) 1))]
      [_ (void)]))

  (define/phase (walk* form*)
    (for-each (λ (s) (walk s)) (syntax->list form*)))
    
  (kernel-syntax-case fpe #f
    [(module ?id ?path (#%plain-module-begin ?form ...))
     (begin
       (walk*/phase #'(?form ...) 0)
       (with-syntax ([(raw-specs ...) (set->list raw-specs)])
         #'(module ?id ?path (#%plain-module-begin (#%require (only raw-specs) ...)))))]))