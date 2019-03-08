#lang racket
(require drracket/tool racket/gui framework)
(provide tool@)

(require "private/main.rkt")


(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define phase2 void)

    (define get-dir<%>
      (interface () get-dir))
    
    (define fc-mixin
      (mixin (racket:text<%> text:autocomplete<%>) ()
        (inherit get-word-at get-start-position)
        (define/override (get-all-words)
          (define col (collections))
          (append
           (for/list ([(k v) (in-hash col)])
             k)
           (set->list (get-completions (get-word-at (get-start-position)) col))
           (super get-all-words)))
        
        (super-new)
        ))

    (define (phase1)
      (drracket:get/extend:extend-definitions-text fc-mixin #f)
      (drracket:get/extend:extend-interactions-text fc-mixin #f))))
