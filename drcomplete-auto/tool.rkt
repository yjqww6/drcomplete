#lang racket
(require drracket/tool racket/gui framework)
(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define phase1 void)
    (define phase2 void)

    (define auto-mixin
      (mixin (racket:text<%> text:autocomplete<%>) ()
        (inherit auto-complete get-start-position get-end-position
                 get-backward-sexp get-text)
        (define cached-pos -1)
              
        (define/override (on-char event)
          (super on-char event)
          (when (and (not (send event get-alt-down))
                     (not (send event get-control-down))
                     (match (send event get-key-code)
                       [(and (? char?) (? char-alphabetic?)) (try-complete)]
                       [#\- (try-complete)]
                       [#\/ (try-complete/)]
                       [_ #f]))
            (auto-complete)))
        (define (try-complete)
          (define start-pos (get-start-position))
          (let ([sexp-pos (get-backward-sexp start-pos)])
            (and sexp-pos
                 (not (= sexp-pos cached-pos))
                 (let ([str (get-text sexp-pos start-pos)])
                   (and (not (< (string-length str) 3))
                        (not (string-prefix? str "'"))
                        (not (string-prefix? str "\""))
                        (not (string-prefix? str "#\""))
                        ))
                 (set! cached-pos sexp-pos)
                 )))
        (define (try-complete/)
          (let* ([start-pos (get-start-position)]
                 [sexp-pos (get-backward-sexp start-pos)]
                 )
            (and sexp-pos
                 (= sexp-pos cached-pos))))
                 

            (super-new)            
                
        ))
    (drracket:get/extend:extend-definitions-text auto-mixin #f)
    (drracket:get/extend:extend-interactions-text auto-mixin #f)
    ))