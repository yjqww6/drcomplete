#lang racket/base
(require (for-syntax racket/base) racket/set "shrink.rkt")

(define-syntax (for/union stx)
  (syntax-case stx ()
    [(_ clauses body ... expr)
     (with-syntax ([orig stx])
       #'(for/fold/derived orig ([s (set)])
           clauses
           body ...
           (set-union s expr)))]))

(define (merge-imports imports)
  (for/union ([p (in-list imports)])
    (list->set (cdr p))))

(define (merge-exports exports)
  (for/union ([p (in-list exports)])
    (list->set (map car (cdr p)))))

(define (walk mod)
  (apply set-union (set mod)
         (map walk (append
                    (module-compiled-submodules mod #f)
                    (module-compiled-submodules mod #t)))))

(define ns (make-base-namespace))

(define (imported-modules fpe)
  (let ([cpl (parameterize ([current-namespace ns])
               (compile fpe))])
    (eval cpl ns)
    (for/union ([mod (in-set (walk cpl))])
      (merge-imports (module-compiled-imports mod)))))

(define (imported-identifiers mods)
  (for/union ([mod (in-set mods)]
              #:when (eval #`(module-declared? #,mod) ns))
    (let-values ([(a b) (eval #`(module->exports #,mod) ns)])
      (set-union (merge-exports a) (merge-exports b)))))

(define (ids fpe)
  (define mods (imported-modules (shrink-module fpe)))
  (imported-identifiers mods))

(provide ids)