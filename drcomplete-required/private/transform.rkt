#lang racket/base
(require (for-syntax racket/base))

(define (walk f specs)
  (filter values (map f (syntax->list specs))))

(define (raw-module-path r)
  (syntax-case r (submod)
    [(submod "." ?id ...) #f]
    [(submod ".." ?id ...) #f]
    [_ #t]))

(define (phaseless-spec spec)
  (syntax-case spec (only prefix all-except prefix-all-except rename)
    [(only ?raw-module-path ?id ...)
     (if (raw-module-path #'?raw-module-path)
         spec
         #f)]
    [(prefix ?prefix-id ?raw-module-path)
     (if (raw-module-path #'?raw-module-path)
         spec
         #f)]
    [(all-except ?raw-module-path ?id ...)
     (if (raw-module-path #'?raw-module-path)
         spec
         #f)]
    [(prefix-all-except ?prefix-id ?raw-module-path ?id ...)
     (if (raw-module-path #'?raw-module-path)
         spec
         #f)]
    [(rename ?raw-module-path _ _)
     (if (raw-module-path #'?raw-module-path)
         spec
         #f)]
    [?raw-module-path
     (if (raw-module-path #'?raw-module-path)
         spec
         #f)]))

(define (raw-require-spec spec)
  (syntax-case spec (for-meta for-syntax for-template for-label just-meta)
    [(for-meta ?level ?phaseless-spec ...)
     #`(for-meta ?level #,@(walk phaseless-spec #'(?phaseless-spec ...)))]
    [(for-syntax ?phaseless-spec ...)
     #`(for-syntax #,@(walk phaseless-spec #'(?phaseless-spec ...)))]
    [(for-template ?phaseless-spec ...)
     #`(for-template #,@(walk phaseless-spec #'(?phaseless-spec ...)))]
    [(for-label ?phaseless-spec ...)
     #`(for-label #,@(walk phaseless-spec #'(?phaseless-spec ...)))]
    [(just-meta ?level ?phaseless-spec ...)
     #`(just-meta ?level #,@(walk phaseless-spec #'(?phaseless-spec ...)))]
    [?phaseless-spec
     (phaseless-spec #'?phaseless-spec)]))

(define (fold-begin forms)
  (filter
   values
   (for/list ([form (in-list forms)])
     (let loop ()
       (syntax-case form (begin begin-for-syntax)
         [(begin) #f]
         [(begin-for-syntax) #f]
         [(begin e ...)
          (let ([b (fold-begin (syntax->list #'(e ...)))])
            (if b
                #`(#,(head form) #,@b)
                #f))]
         [(begin-for-syntax e ...)
          (let ([b (fold-begin (syntax->list #'(e ...)))])
            (if b
                #`(#,(head form) #,@b)
                #f))]
         [_ form])))))

(define (head stx)
  (syntax-case stx ()
    [(a . b) #'a]))

(define (shrink-module fpe)
  (let ([fpe (syntax-disarm fpe #f)])
    (syntax-rearm
     (syntax-case* fpe
       (module module* #%require begin begin-for-syntax)
       (λ (a b) (free-identifier=? a b #f #f))
       [(module ?id ?path (#%plain-module-begin ?form ...))
        #`(#,(head fpe)
           ?id ?path
           (#%plain-module-begin
            #,@(fold-begin (map shrink-module (syntax->list #'(?form ...))))))]
       [(module* ?id ?path (#%plain-module-begin ?form ...))
        #`(#,(head fpe) ?id ?path
                        (#%plain-module-begin
                         #,@(fold-begin (map shrink-module (syntax->list #'(?form ...))))))]
       [(#%require ?spec ...)
        #`(#,(head fpe) #,@(walk phaseless-spec #'(?spec ...)))]
       [(begin ?form ...)
        #`(#,(head fpe) #,@(fold-begin (map shrink-module (syntax->list #'(?form ...)))))]
       [(begin-for-syntax ?form ...)
        #`(#,(head fpe)
           #,@(fold-begin (map shrink-module
                               (syntax->list #'(?form ...)))))]
       [_ #'(begin)])
     fpe)))

(provide shrink-module)
