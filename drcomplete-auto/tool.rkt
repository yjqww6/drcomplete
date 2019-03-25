#lang racket
(require drracket/tool racket/gui framework)
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
          (and (>= (string-length str) 3)
               (match* ((string-ref str 0)(string-ref str 1))
                 [((or #\' #\" #\`) _) #f]
                 [(#\# (or #\' #\` #\, #\%)) #t]
                 [(#\# _) #f]
                 [(_ _) #t])))
        
        (define soft-cached-pos -1)
        (define cached-pos -1)
        (define on-char? #f)
        
        (define/override (on-char event)
          (when (match (send event get-key-code)
                  [(or 'shift 'rshift) #f]
                  [_ #t])
            (dynamic-wind
             (λ () (set! on-char? #t))
             (λ () (super on-char event))
             (λ () (set! on-char? #f))))
          (when (and (preferences:get 'drcomplete:auto-completion)
                     (not (send event get-alt-down))
                     (not (send event get-control-down))
                     (not (send event get-meta-down)))
            (match (send event get-key-code)
              [(or (and (? char?) (? char-alphabetic?)) #\- #\: #\+
                   #\*)
               (when (try-complete)
                 (auto-complete))]
              [#\/
               (when (try-complete/)
                 (super on-char (new key-event%))
                 (auto-complete))]
              [_ (void)])))

        (define/augment (after-set-position)
          (when (not on-char?)
            (super on-char (new key-event%)))
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
                 
                 (need-completion? (get-text sexp-pos start-pos))
                        
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