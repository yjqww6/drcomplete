#lang racket
(require drracket/tool racket/gui framework racket/runtime-path)
(provide tool@)


(define-runtime-path expansion.rkt "private/expansion.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define phase1 void)

    (define-local-member-name set-required-identifiers)
    
    (define rc-mixin
      (mixin (racket:text<%> text:autocomplete<%>) ()
        (super-new)

        (define required '())

        (define/public (set-required-identifiers ls)
          (set! required ls))

        (define/override (get-all-words)
          required)
        ))

    
    (drracket:module-language-tools:add-online-expansion-handler
     expansion.rkt 'go
     (λ (t v)
       (when v
         (send t set-required-identifiers v)
         (send (send (send t get-tab) get-ints)
               set-required-identifiers v))))

    (define (phase2)
      (drracket:get/extend:extend-interactions-text rc-mixin #f)
      (drracket:get/extend:extend-definitions-text rc-mixin #f))
    ))
