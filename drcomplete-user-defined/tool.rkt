#lang racket
(require drracket/tool racket/gui framework racket/runtime-path
         syntax-color/module-lexer
         syntax-color/lexer-contract
         drcomplete-base
         racket/symbol)
(provide tool@)

(define (symbols in)
  (let loop ([mode #f] [s (set)])
    (define-values (str type _1 _2 _3 _4 new-mode)
      (module-lexer in 0 mode))
    (define next-mode
      (if (dont-stop? new-mode)
          (dont-stop-val new-mode)
          new-mode))
    (cond
      [(or (eof-object? str) (eq? type 'eof)) s]
      [(and (eq? type 'symbol) (string? str))
       (loop next-mode (set-add s str))]
      [else (loop next-mode s)])))

(define-runtime-path expansion.rkt "private/expansion.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define phase2 void)

    (define-local-member-name set-user-defined-identifiers repl-identifiers)
    
    (define udc-mixin
      (mixin (racket:text<%> text:autocomplete<%>) ()
        (super-new)

        (define user-defined (set))

        (define/public (set-user-defined-identifiers ls)
          (set! user-defined (list->set ls)))

        (define/override (get-all-words)
          (set-union user-defined (super get-all-words)))
        ))

    (define def-mixin
      (mixin (racket:text<%> text:autocomplete<%>) ()
        (inherit get-text get-word-at get-start-position)
        (super-new)

        (define/override (get-all-words)
          (set-union
           (set-remove (symbols (open-input-string (get-text)))
                       (get-word-at (get-start-position)))
           (super get-all-words)))))

    (define rep-mixin
      (mixin (drracket:rep:text<%> text:autocomplete<%>) ()
        (inherit get-definitions-text run-in-evaluation-thread)
        (super-new)
        (define repl-defined (set))

        (define/augment (after-many-evals)
          (run-in-evaluation-thread
           (λ ()
             (set! repl-defined (for/set ([s (in-list (namespace-mapped-symbols (current-namespace)))])
                                  (symbol->immutable-string s)))))
          (inner (void) after-many-evals))

        (define/override (get-all-words)
          (define defs (get-definitions-text))
          (if (is-a? defs drracket:unit:definitions-text<%>)
              (set-union
               (symbols (open-input-string (send defs get-text)))
               repl-defined
               (super get-all-words))
              (super get-all-words)))))

    
    (drracket:module-language-tools:add-online-expansion-handler
     expansion.rkt 'go
     (λ (t v)
       (when v
         (send t set-user-defined-identifiers v)
         (send (send (send t get-tab) get-ints)
               set-user-defined-identifiers v))))

    (define (phase1)
      (register-drcomplete-plugin
       #:def (compose def-mixin udc-mixin)
       #:int (compose rep-mixin udc-mixin)))
    ))
