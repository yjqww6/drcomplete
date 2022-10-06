#lang racket/base
(require racket/set racket/sequence syntax/kerncase "utils.rkt")

(cond-use-bound
 (define (walk form phase mods)
   (kernel-syntax-case/phase
    form phase
    [(module ?id ?path (_ ?form ...))
     (walk* #'(?form ...) 0 (cons #'?path mods))]
    [(module* ?id ?f (_ ?form ...))
     (eq? (syntax-e #'?f) #f)
     (walk* #'(?form ...) phase (cons #'?f mods))]
    [(module* ?id ?path (_ ?form ...))
     (walk* #'(?form ...) 0 (cons #'?path mods))]
    [(begin ?form ...)
     (walk* #'(?form ...) phase mods)]
    [(begin-for-syntax ?form ...)
     (walk* #'(?form ...) (add1 phase) mods)]
    [_ mods]))

 (define (walk* form* phase mods)
   (for/fold ([mods mods])
             ([form (in-syntax form*)])
     (walk form phase mods)))

 (define (walk-module fpe)
   (define mods
     (kernel-syntax-case fpe #f
       [(module ?id ?path (#%plain-module-begin ?form ...))
        (walk* #'(?form ...) (namespace-base-phase) (list #'?path))]))
   (define ids (mutable-set))
   (for* ([mod (in-list mods)]
          #:when (visible? mod)
          [phase (in-list (syntax-bound-phases mod))]
          [sym (in-list (syntax-bound-symbols mod phase))])
     (set-add! ids sym))
   ids)

 (provide walk-module))