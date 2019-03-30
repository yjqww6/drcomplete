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
                 get-backward-sexp get-forward-sexp get-text
                 get-word-at)

        (define soft-cached-pos -1)
        (define cached-pos -1)
        (define on-char? #f)

        (define thunk #f)
        (define ts 0)

        (define thr
          (thread
           (λ ()
             (with-handlers ([exn:break? void])
               (let loop ()
                 (sleep 0.1)
                 (when (and thunk
                            (= (car thunk) ts)
                            (> (- (current-milliseconds) ts) 0.4))
                   (queue-callback (cdr thunk))
                   (set! thunk #f)
                   (sleep 0.4))
                 (loop))))))

        (define/augment (on-close)
          (break-thread thr)
          (inner (void) on-close))
        
        (define/override (on-char event)
          (define t (current-milliseconds))
          (set! ts t)
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
               (set! thunk
                     (cons
                      t
                      (λ ()
                        (when (and (= t ts) (try-complete))
                          (auto-complete)))))]
              [#\/
               (set! thunk
                     (cons
                      t
                      (λ ()
                        (when (and (= t ts) (try-complete/))
                          (super on-char (new key-event%))
                          (auto-complete)))))]
              [_ (set! thunk #f)])))

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
          (define word (get-word-at start-pos))
          (define word-pos (max 0 (- start-pos (string-length word))))
          (and (not (= word-pos cached-pos))
               (not (string=? "\"" (get-text (max 0 (- word-pos 1)) word-pos)))
               (>= (string-length word) 3)
               (set! soft-cached-pos word-pos)
               (set! cached-pos word-pos)))
        
        (define/private (try-complete/)
          (let* ([start-pos (get-start-position)]
                 [word (get-word-at start-pos)]
                 [word-pos (- start-pos (string-length word))])
            (= word-pos soft-cached-pos)))
        
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
