#lang racket
(require drracket/tool racket/gui framework drcomplete-base)
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
          (set-union
           (for/set ([(k v) (in-hash col)])
             k)
           (get-completions (get-word-at (get-start-position)) col)
           (super get-all-words)))
        
        (super-new)
        ))

    (define (phase1)
      (register-drcomplete-plugin
       #:def fc-mixin #:int fc-mixin))))
