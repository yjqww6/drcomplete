#lang racket
(require drracket/tool framework drcomplete-base)
(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define phase2 void)

    (define-local-member-name set-method-names)
    
    (define method-names
      (mixin (racket:text<%> text:autocomplete<%>) ()
        (super-new)

        (define method-names (set))

        (define/public (set-method-names ls)
          (set! method-names (list->set ls)))

        (define/override (get-all-words)
          (set-union method-names (super get-all-words)))
        ))

    (define (rep-mixin %)
      (class %
        (inherit get-definitions-text run-in-evaluation-thread
                 get-user-namespace set-method-names)
        (super-new)

        (define first-run? #f)
        
        (define/augment (after-many-evals)
          (cond
            [(drracket:rep:module-language-initial-run)
             (set! first-run? #t)]
            [first-run?
             (set! first-run? #f)
             (run-in-evaluation-thread
              (λ ()
                (define ns (current-namespace))
                (define (->inteface c)
                  (cond
                    [(interface? c) c]
                    [(class? c) (class->interface c)]
                    [else #f]))
                (when ns
                  (define syms
                    (for/fold ([s (set)])
                              ([sym (in-list (namespace-mapped-symbols ns))])
                      (define val (namespace-variable-value sym #t (λ () #f) ns))
                      (cond
                        [(->inteface val)
                         =>
                         (λ (<%>)
                           (define mn (interface->method-names <%>))
                           (set-union s (list->set (map symbol->string mn))))]
                        [else s])))
                  (set-method-names syms)
                  (send (get-definitions-text) set-method-names syms))))]
            [else (void)])
          (inner (void) after-many-evals))))

    (define (phase1)
      (register-drcomplete-plugin
       #:def method-names
       #:int (compose rep-mixin method-names)))
    ))
