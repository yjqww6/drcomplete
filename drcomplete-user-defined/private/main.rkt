#lang racket/base
(require racket/set syntax/kerncase)
(provide walk)

(define (walk* stxs phase)
  (let loop ([ls (syntax->list stxs)])
    (if (null? ls)
        (seteq)
        (set-union (walk (car ls) phase) (loop (cdr ls))))))

(define (visible? id)
  (for/and ([scope (in-list
                    (hash-ref (syntax-debug-info id)
                              'context (λ () '())))])
    (not (eq? 'macro (vector-ref scope 1)))))

(define (visible stx)
  (syntax-case stx ()
    [(a . b)
     (set-union (visible #'a) (visible #'b))]
    [x
     (identifier? #'x)
     (if (visible? #'x)
         (seteq (syntax-e #'x))
         (seteq))]
    [_ (seteq)]))

(define (walk stx [phase 0])
  (set-union
   (kernel-syntax-case/phase
    stx phase
    [(#%expression ?expr) (walk #'?expr phase)]
    [(#%plain-module-begin ?module-level-form ...)
     (walk* #'(?module-level-form ...) phase)]
    [(module _ _ ?plain)
     (walk* #'?plain 0)]
    [(module* _ _ ?plain)
     (walk* #'?plain 0)]
    [(begin ?expr ...)
     (walk* #'(?expr ...) phase)]
    [(begin0 ?expr ...)
     (walk* #'(?expr ...) phase)]
    [(begin-for-syntax ?expr ...)
     (walk* #'(?expr ...) (+ phase 1))]
    [(define-values (?id ...) ?expr)
     (set-union (visible #'(?id ...)) (walk #'?expr phase))]
    [(define-syntaxes (?id ...) ?expr)
     (set-union (visible #'(?id ...)) (walk #'?expr (+ phase 1)))]
    [(#%plain-lambda ?formals ?expr ...)
     (set-union (visible #'?formals) (walk* #'(?expr ...) phase))]
    [(case-lambda (?formals ?expr ...) ...)
     (set-union (visible #'(?formals ...)) (walk* #'(?expr ... ...) phase))]
    [(if ?expr ...)
     (walk* #'(?expr ...) phase)]
    [(let-values ([(?id ...) ?expr] ...)
       ?body ...)
     (set-union (visible #'(?id ... ...))
                (walk* #'(?expr ... ?body ...) phase))]
    [(letrec-values ([(?id ...) ?expr] ...)
       ?body ...)
     (set-union (visible #'(?id ... ...))
                (walk* #'(?expr ... ?body ...) phase))]
    [(set! ?id ?expr)
     (walk #'?expr phase)]
    [(with-continuation-mark ?expr ...)
     (walk* #'(?expr ...) phase)]
    [(#%plain-app ?expr ...)
     (walk* #'(?expr ...) phase)]
    [_ (seteq)])
   (visible (syntax-property stx 'disappeared-binding))))