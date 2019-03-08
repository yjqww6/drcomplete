#lang racket
(require drracket/tool racket/gui framework racket/runtime-path
         syntax-color/module-lexer)
(provide tool@)

(define (symbols in)
  (define s (set))
  (let loop ([mode #f])
    (define-values (str type _1 _2 _3 _4 new-mode)
      (module-lexer in 0 mode))
    (cond
      [(eof-object? str) s]
      [(eq? type 'symbol)
       (set! s (set-add s str))
       (loop new-mode)]
      [else (loop new-mode)])))

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

    (define def-mixin
      (mixin (racket:text<%> text:autocomplete<%>) ()
        (inherit get-text get-word-at get-start-position)
        (super-new)

        (define/override (get-all-words)
          (set->list
           (set-union
            (set-remove (symbols (open-input-string (get-text)))
                        (get-word-at (get-start-position)))
            (list->set (super get-all-words)))))))

    (define rep-mixin
      (mixin (drracket:rep:text<%> text:autocomplete<%>) ()
        (inherit get-definitions-text)
        (super-new)

        (define/override (get-all-words)
          (define defs (get-definitions-text))
          (if (is-a? defs drracket:unit:definitions-text<%>)
              (set->list
               (set-union
                (symbols (open-input-string (send defs get-text)))
                (list->set (super get-all-words))))
              (super get-all-words)))))

    
    (drracket:module-language-tools:add-online-expansion-handler
     expansion.rkt 'go
     (λ (t v)
       (when v
         (send t set-user-defined-identifiers v)
         (send (send (send t get-tab) get-ints)
               set-user-defined-identifiers v))))

    (define (phase1)
      (drracket:get/extend:extend-interactions-text (compose rep-mixin udc-mixin) #f)
      (drracket:get/extend:extend-definitions-text (compose def-mixin udc-mixin) #f))
    ))
