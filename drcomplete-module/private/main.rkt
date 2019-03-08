#lang typed/racket
(require/typed syntax/modread
               [with-module-reading-parameterization
                 (All (a) (-> a) -> a)])

(define (list-collection-path [path : Path])
  (for/set : (Setof (Pairof String Path))
    ([d (in-list (directory-list path #:build? #t))]
     #:when (directory-exists? d))
    (define-values (b f s) (split-path d))
    (assert f path?)
    (cons (path->string f) d)))

(define (list-collection-link [link : Path])
  (for/fold
   ([s : (Setof (Pairof String Path)) (set)])
   ([l (in-list
        (with-module-reading-parameterization
          (λ ()
            (call-with-input-file link read))))])
    (match l
      [(list* (or 'root 'static-root) (? path-string? p) (or (list (? regexp? reg))
                                                      (? null? reg)))
       (let ([p (simplify-path (build-path link 'up p))])
         (if (and reg
                  (or (null? reg) (regexp-match? reg (version)))
                  (directory-exists? p))
             (set-union s (list-collection-path p))
             s))]
      [(list* (? string? k) (? path-string? p) (or (list (? regexp? reg))
                                            (? null? reg)))
       (let ([p (simplify-path (build-path link 'up p))])
         (if (and reg
                  (or (null? reg) (regexp-match? reg (version)))
                  (directory-exists? p))
             (set-add s (cons k p))
             s))])))

(define (collections)
  (define s : (Setof (Pairof String Path))
    (let loop ([ls (current-library-collection-links)])
      (match ls
        ['() (set)]
        [`(#f . ,r)
         (set-union
          (for/fold ([s : (Setof (Pairof String Path)) (set)])
                    ([p (in-list (current-library-collection-paths))]
                     #:when (directory-exists? p))
            (set-union s (list-collection-path p)))
          (loop r))]
        [(cons (? path? p) r)
         (set-union
          (list-collection-link p)
          (loop r))]
        [(cons (? hash? h) r)
         (for*/fold ([s : (Setof (Pairof String Path)) (loop r)])
                    ([(k v) (in-hash h)]
                     [p (in-list v)])
           (if k
               (set-add s (cons (symbol->string k) p))
               (set-union s (list-collection-path p))))])))
  (for/fold ([h : (HashTable String (Setof Path)) (hash)])
            ([p (in-set s)])
    (hash-update h (car p)
                 (λ ([s : (Setof Path)])
                   (set-add s (cdr p)))
                 (λ () (set (cdr p))))))

(define (get-completions [str : String]
                         [cols : (HashTable String (Setof Path))
                               (collections)])
  : (Setof String)
  (match (string-split str "/" #:trim? #f)
    [(and frags (list col path path* ...))
     (define (join-str [f : String])
       (string-join (append (drop-right frags 1) (list f)) "/"))
     (let ([dirs (hash-ref cols col (λ () #f))])
       (cond
         [(not dirs) (set)]
         [else
          (for/fold ([s : (Setof String) (set)])
                    ([dir (in-set dirs)])
            (let loop ([path^ (cons path path*)] [dir dir])
              (match path^
                [(list p)
                 (for/fold ([s : (Setof String) s])
                           ([f (in-list (directory-list dir))])
                   (cond
                     [(not (string-prefix? (path->string f) p)) s]
                     [(and (file-exists? (build-path dir f))
                           (member (path-get-extension f)
                                   '(#".rkt")))
                      (set-add
                       s
                       (join-str
                        (path->string
                         (path-replace-extension f #""))))]
                     [(directory-exists? (build-path dir f))
                      (set-add
                       s
                       (join-str (path->string f)))]
                     [else s]))]
                [(cons p path*)
                 (for/fold ([s s])
                           ([d (in-list (directory-list dir #:build? #t))]
                            #:when (directory-exists? d))
                   (set-union
                    s
                    (loop path* d)))]
                [else s])))]))]
    [else (set)]))

(provide collections get-completions)