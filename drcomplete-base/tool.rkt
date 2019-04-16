#lang racket
(require drracket/tool racket/gui framework racket/runtime-path)
(provide tool@)


(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
    (define phase1 void)
    
    (define (phase2)
      (let loop ([defs '()] [ints '()])
        (match (sync/timeout 0 plugin-receiver)
          [(vector 'info _  (cons  d i) 'drcomplete-plugin)
           (loop (cons d defs) (cons i ints))]
          [else
           (register-drcomplete-mixin defs ints)])))

    (define l->s-mixin
      (mixin (racket:text<%> text:autocomplete<%>) ()
        (define/override (get-all-words)
          (list->set (super get-all-words)))
        (super-new)))

    (define s->l-mixin
      (mixin (racket:text<%> text:autocomplete<%>) ()
        (define/override (get-all-words)
          (set->list (super get-all-words)))
        (super-new)))

    (define plugin-receiver (make-log-receiver (current-logger)
                                               'info 'drcomplete-plugin))

    (define (register-drcomplete-mixin defs ints)
      (define (comp l)
        (compose s->l-mixin (apply compose (map cdr (sort l > #:key car))) l->s-mixin))
      (drracket:get/extend:extend-definitions-text (comp defs))
      (drracket:get/extend:extend-interactions-text (comp ints)))
    
    ))
