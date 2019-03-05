#lang racket
(require drracket/tool racket/gui framework racket/runtime-path)
(provide tool@)


(define-runtime-path expansion.rkt "private/expansion.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define phase2 void)

    (define-local-member-name set-user-defined-identifiers)
    
    (define udc-mixin
      (mixin (racket:text<%> text:autocomplete<%>) ()
        (super-new)

        (define user-defined '())

        (define/public (set-user-defined-identifiers ls)
          (set! user-defined ls))

        (define/override (get-all-words)
          (append user-defined (super get-all-words)))
        ))

    
    (drracket:module-language-tools:add-online-expansion-handler
     expansion.rkt 'go
     (λ (t v)
       (when v
         (send t set-user-defined-identifiers v)
         (send (send (send t get-tab) get-ints)
               set-user-defined-identifiers v))))

    (define (phase1)
      (drracket:get/extend:extend-interactions-text udc-mixin #f)
      (drracket:get/extend:extend-definitions-text udc-mixin #f))
    ))
