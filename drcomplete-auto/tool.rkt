#lang racket
(require drracket/tool racket/gui framework)
(require racket/unsafe/ops)
(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define phase1 void)
    (define phase2
      (thunk
       (preferences:set-default
        'drcomplete:auto-completion
        #t
        boolean?)))

    (define auto-mixin
      (mixin (racket:text<%> text:autocomplete<%>) ()
        (inherit auto-complete get-start-position get-end-position
                 get-backward-sexp get-forward-sexp get-text)
        
        (define (need-completion? str)
          (let ([char1 (unsafe-string-ref str 0)]
                [char2 (unsafe-string-ref str 1)])
            (cond [(char=? char1 #\') #t] ;;quote
                  [(char=? char1 #\`) #t] ;;quasiquote
                  [(char=? char1 #\") #t] ;;string
                  [(char=? char1 #\#)
                   (or (char=? char2 #\')
                       (char=? char2 #\`)
                       (char=? char2 #\,)
                       ) ;;byte string, keyword
                   ;;regexp,numbers ... except #' #` #, #,@
                   ])))
        
        (define soft-cached-pos -1)
        (define cached-pos -1)
              
        (define/override (on-char event)
          (super on-char event)
          (when (and (preferences:get 'drcomplete:auto-completion)
                     (not (send event get-alt-down))
                     (not (send event get-control-down)))
            (match (send event get-key-code)
              [(or (and (? char?) (? char-alphabetic?)) #\-)
               (when (try-complete)
                 (auto-complete))]
              [(or #\/ #\:)
               (when (try-complete/)
                 (super on-char (new key-event%))
                 (auto-complete))]
              [_ (void)])))

        (define/augment (after-set-position)
          (super on-char (new key-event%))
          (inner (void) after-set-position))

        (define/augment (after-insert start len)
          (when (= start cached-pos)
            (set! cached-pos -1))
          (inner (void) after-insert start len))
        
        (define/private (try-complete)
          (define start-pos (get-start-position))
          (let ([sexp-pos (get-backward-sexp start-pos)])
            (and sexp-pos
                 (not (= sexp-pos cached-pos))
                 
                 ;inside comment
                 (let ([next-pos (get-forward-sexp sexp-pos)])
                   (and next-pos (<= start-pos next-pos)))
                 
                 (let ([str (get-text sexp-pos start-pos)])
                   (and (>= (string-length str) 3)
                        (need-completion? str)
                        ))
                 (set! soft-cached-pos sexp-pos)
                 (set! cached-pos sexp-pos))))
        
        (define/private (try-complete/)
          (let* ([start-pos (get-start-position)]
                 [sexp-pos (get-backward-sexp start-pos)])
            (and sexp-pos
                 (= sexp-pos soft-cached-pos))))
        
        (super-new)
                
        ))

    (define frame-mixin
      (mixin (frame:standard-menus<%>) ()
        (super-new)
        (inherit edit-menu:after-preferences get-edit-menu)
        (define switch
          (new menu-item% [label ""] [parent (get-edit-menu)]
               [callback
                (λ (c e)
                  (preferences:set
                   'drcomplete:auto-completion
                   (not
                    (preferences:get 'drcomplete:auto-completion))))]
               [demand-callback
                (λ (c)
                  (send switch set-label
                        (if (preferences:get 'drcomplete:auto-completion)
                            "Disable Automatic AutoCompletion"
                            "Enable Automatic AutoCompletion")))]))
        (edit-menu:after-preferences switch)))
    
    (drracket:get/extend:extend-definitions-text auto-mixin #f)
    (drracket:get/extend:extend-interactions-text auto-mixin #f)
    (drracket:get/extend:extend-unit-frame frame-mixin #f)
    ))