#lang racket
(require (for-syntax racket/base racket/syntax) syntax/kerncase racket/stxparam)
(provide walk-module)

(define-syntax-parameter phase-id (syntax-rules ()))

(define-syntax (for/union stx)
  (syntax-case stx ()
    [(_ clauses body ... expr)
     (with-syntax ([orig stx])
       #'(for/fold/derived orig ([s (set)])
           clauses
           body ...
           (set-union s expr)))]))

(define-syntax (define/phase stx)
  (syntax-case stx ()
    [(_ (f arg ...) body ...)
     (with-syntax ([f/p (format-id #'f "~a/phase" (syntax-e #'f))])
       #'(begin (define (f/p arg ... phase)
                  (syntax-parameterize
                      ([phase-id
                        (syntax-rules () [(_) phase])])
                    body ...))
                (define-syntax-rule (f arg ...)
                  (f/p arg ... (phase-id)))))]))

(define (sym=? a b)
  (free-identifier=? a b #f #f))

(define (walk-module fpe)

  (define declared-modules (mutable-set))
  
  (define ids (mutable-set))
  
  (define alls (mutable-set))
  (define prefixs (mutable-set))
  (define all-excepts (mutable-set))
  (define prefix-all-excepts (mutable-set))

  (define (push! sth)
    (when (ext-module-path? sth)
      (set-add! declared-modules (syntax->datum sth))))

  (define (ext-module-path? r)
    (syntax-case* r (submod quote)
      sym=?
      [(submod "." _ ...) #f]
      [(submod ".." _ ...) #f]
      [(submod (quote _) _ ...) #f]
      [(quote _) #f]
      [_ #t]))

  (define (phaseless-spec spec just)
    (define-syntax-rule (with-datum ([id0 exp0] [id exp] ...) body ...)
      (let ([id0 (syntax->datum exp0)])
        (when (ext-module-path? id0)
          (let ([id (syntax->datum exp)] ...)
            body ...))))
    
    (syntax-case* spec
      (only prefix all-except prefix-all-except rename)
      sym=?
      [(only ?raw-module-path ?id ...)
       (with-datum ([mod #'?raw-module-path]
                    [ids #'(?id ...)])
         (for ([id (in-list ids)])
           (set-add! ids id)))]
      [(prefix ?prefix-id ?raw-module-path)
       (with-datum ([mod #'?raw-module-path]
                    [pre #'?prefix-id])
         (set-add! declared-modules mod)
         (set-add! prefixs (list* just mod pre)))]
      [(all-except ?raw-module-path ?id ...)
       (with-datum ([mod #'?raw-module-path]
                    [ids #'(?id ...)])
         (set-add! declared-modules mod)
         (set-add! all-excepts (list* just mod ids)))]
      [(prefix-all-except ?prefix-id ?raw-module-path ?id ...)
       (with-datum ([mod #'?raw-module-path]
                    [pre #'?prefix-id]
                    [ids #'(?id ...)])
         (set-add! declared-modules mod)
         (set-add! prefix-all-excepts (list* just mod pre ids)))]
      [(rename ?raw-module-path ?id _)
       (with-datum ([mod #'?raw-module-path]
                    [id #'?id])
         (set-add! ids id))]
      [?raw-module-path
       (with-datum ([mod #'?raw-module-path])
         (set-add! declared-modules mod)
         (set-add! alls (cons just mod)))]))

  (define (each f syn . args)
    (for ([s (in-syntax syn)])
      (apply f s args)))
    
  (define (raw-require-spec spec)
    (define (maybe-just-meta spec)
      (syntax-case* spec (just-meta) sym=?
        [(just-meta ?n ?phaseless-spec* ...)
         (let ([n (syntax-e #'?n)])
           (when n
             (each phaseless-spec #'(?phaseless-spec* ...) n)))]
        [?phaseless-spec
         (phaseless-spec #'?phaseless-spec #f)]))

    (define (maybe-shift spec just)
      (syntax-case* spec
        (for-meta for-syntax for-template for-label)
        sym=?
        [(for-meta ?level ?phaseless-spec ...)
         (let ([level (syntax-e #'?level)])
           (each phaseless-spec #'(?phaseless-spec ...)
                 (- just (or level 0))))]
        [(for-syntax ?phaseless-spec ...)
         (each phaseless-spec #'(?phaseless-spec ...) (- just 1))]
        [(for-template ?phaseless-spec ...)
         (each phaseless-spec #'(?phaseless-spec ...) (- just -1))]
        [(for-label ?phaseless-spec ...)
         (each phaseless-spec #'(?phaseless-spec ...) just)]
        [?phaseless-spec
         (phaseless-spec #'?phaseless-spec just)]))
    
    (syntax-case* spec
      (for-meta for-syntax for-template for-label just-meta)
      sym=?
      [(for-meta ?level ?phaseless-spec ...)
       (each maybe-just-meta #'(?phaseless-spec ...))]
      [(for-syntax ?phaseless-spec ...)
       (each maybe-just-meta #'(?phaseless-spec ...))]
      [(for-template ?phaseless-spec ...)
       (each maybe-just-meta #'(?phaseless-spec ...))]
      [(for-label ?phaseless-spec ...)
       (each maybe-just-meta #'(?phaseless-spec ...))]
      [(just-meta ?level ?raw-require-spec ...)
       (let ([level (syntax-e #'?level)])
         (when level
           (each maybe-shift #'(?raw-require-spec ...) level)))]
      [?phaseless-spec
       (phaseless-spec #'?phaseless-spec #f)]))

  (define modu #'begin-for-syntax)
  
  (define/phase (walk form)
    (syntax-case* form (module module* #%require begin begin-for-syntax)
      (λ (a b)
        (or
         (free-identifier=? a b (phase-id) 0)
         (free-identifier=? a b)))
      [(module ?id ?path (#%plain-module-begin ?form ...))
       (begin
         (phaseless-spec #'?path #f)
         (walk* #'(?form ...)))]
      [(module* ?id ?path (#%plain-module-begin ?form ...))
       (begin
         (when (syntax-e #'?path)
           (phaseless-spec #'?path #f))
         (walk* #'(?form ...)))]
      [(#%require ?spec ...)
       (for ([spec (in-syntax #'(?spec ...))])
         (raw-require-spec spec))]
      [(begin ?form ...)
       (walk* #'(?form ...))]
      [(begin-for-syntax ?form ...)
       (walk*/phase #'(?form ...) (+ (phase-id) 1))]
      [_ (void)]))

  (define/phase (walk* form*)
    (for-each (λ (s) (walk s)) (syntax->list form*)))
    
  (kernel-syntax-case fpe #f
    [(module ?id ?path (#%plain-module-begin ?form ...))
     (begin
       (phaseless-spec #'?path #f)
       (walk*/phase #'(?form ...) 0)
       
       (with-syntax ([(raw-specs ...) (set->list declared-modules)])
         (define ns (make-base-namespace))
         
         (define (get-exports mod just)
           (define (filter-exports exports)
             (cond
               [(not just) (for/union ([p (in-list exports)])
                             (list->set (map car (cdr p))))]
               [(assq just exports)
                =>
                (λ (p)
                  (list->set (map car (cdr p))))]
               [else (set)]))
           (let-values ([(a b) (eval #`(module->exports '#,mod) ns)])
             (set-union (filter-exports a) (filter-exports b))))
         
         (eval
          #'(module ?id ?path (#%plain-module-begin (#%require (only raw-specs) ...)))
          ns)
         
         (for ([jm (in-set alls)])
           (for ([id (in-set (get-exports (cdr jm) (car jm)))])
             (set-add! ids id)))
         (for ([jm (in-set prefixs)])
           (for ([id (in-set (get-exports (cadr jm) (car jm)))])
             (set-add! ids (string->symbol (string-append (symbol->string (cddr jm))
                                                          (symbol->string id))))))
         (for ([jm (in-set all-excepts)])
           (define e (foldl (λ (v s) (set-remove s v)) (get-exports (cadr jm) (car jm)) (cddr jm)))
           (for ([id (in-set e)])
             (set-add! ids id)))

         (for ([jm (in-set prefix-all-excepts)])
           (define e (foldl (λ (v s) (set-remove s v)) (get-exports (cadr jm) (car jm)) (cdddr jm)))
           (for ([id (in-set e)])
             (set-add! ids (string->symbol (string-append (symbol->string (caddr jm))
                                                          (symbol->string id))))))))])
  ids)