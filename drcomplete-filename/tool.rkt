#lang racket
(require drracket/tool racket/gui framework srfi/2)
(provide tool@)

(require "private/main.rkt" "interface.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define phase1 void)
    (define phase2 void)
    
    (define fc-mixin
      (mixin (racket:text<%> text:autocomplete<%>) (get-dir<%>)
        (inherit get-text get-backward-sexp get-start-position)

        (define/private (check-path pos)
          (and-let*
           ([start (get-backward-sexp pos)]
            [str (get-text start pos)]
            [start (cond
                     [(string-prefix? str "\"")
                      (+ 1 start)]
                     [(string-prefix? str "#\"")
                      (+ 2 start)]
                     [else #f])]
            [str (get-text start pos)])
           (cond
             [(path-string? str) str]
             [(string=? str "") ""]
             [else #f])))
        
        (define/override (get-word-at pos)
          (cond
            [(check-path pos) => values]
            [else (super get-word-at pos)]))
        
        (define/public (drcomplete:get-dir)
          (current-directory))

        (define/public (drcomplete:path-completions [pos (get-start-position)])
          (and-let*
           ([str (check-path pos)])
           (parameterize ([current-directory (drcomplete:get-dir)])
             (get-completions str))))
        
        (define/override (get-all-words)
          (or
           (drcomplete:path-completions (get-start-position))
           (super get-all-words)))
        
        (super-new)
        ))

    (define def-mixin
      (mixin (drracket:unit:definitions-text<%> get-dir<%>) ()
        (inherit get-tab)
        (define/override (drcomplete:get-dir)
          (or
           (let ([t (get-tab)])
             (send t get-directory))
           (current-directory)))
        (super-new)))

    (define rep-mixin
      (mixin (drracket:rep:text<%> get-dir<%>) ()
        (inherit run-in-evaluation-thread)
        
        (define/override (drcomplete:get-dir)
          (or dir (current-directory)))

        (define dir #f)

        (define/augment (after-many-evals)
          (run-in-evaluation-thread (λ () (set! dir (current-directory))))
          (inner #f after-many-evals))
        
        (super-new)))
    
    (drracket:get/extend:extend-definitions-text (compose def-mixin fc-mixin))
    (drracket:get/extend:extend-interactions-text (compose rep-mixin fc-mixin))))
