#lang racket
(require drracket/tool racket/gui framework srfi/2)
(provide tool@)

(require "private/main.rkt")

(define-local-member-name get-dir)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define phase1 void)
    (define phase2 void)
    
    (define fc-mixin
      (mixin (racket:text<%> text:autocomplete<%>) ()
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

        (define/public (get-dir)
          (current-directory))
        
        (define/override (get-all-words)
          (or
           (and-let*
            ([str (check-path (get-start-position))])
            (parameterize ([current-directory (get-dir)])
              (get-completions str)))
           (super get-all-words)))
        
        (super-new)
        ))

    (define (def-mixin %)
      (class %
        (inherit get-filename)
        (define/override (get-dir)
          (define b (box #f))
          (define p (get-filename b))
          (or (and (not (unbox b)) p
                   (let-values ([(b s) (parse-path p)])
                     b))
              (super get-dir)))
        (super-new)))
    
    (drracket:get/extend:extend-definitions-text (compose def-mixin fc-mixin))
    (drracket:get/extend:extend-interactions-text fc-mixin)))
