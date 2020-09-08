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

        (define/public (set-method-names s)
          (set! method-names s))

        (define/override (get-all-words)
          (set-union method-names (super get-all-words)))
        ))

    (define (rep-mixin %)
      (class %
        (inherit get-definitions-text set-method-names)
        (super-new)

        (define/override (evaluate-from-port port complete-program? cleanup)
          (super evaluate-from-port port complete-program?
                 (if complete-program?
                     (λ ()
                       (cleanup)
                       (define ns (current-namespace))
                       (define (->inteface c)
                         (cond
                           [(interface? c) c]
                           [(class? c) (class->interface c)]
                           [else #f]))
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
                       (send (get-definitions-text) set-method-names syms))
                     cleanup)))))

    (define (phase1)
      (register-drcomplete-plugin
       #:def method-names
       #:int (compose rep-mixin method-names)))
    ))
