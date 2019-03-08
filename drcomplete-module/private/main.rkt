#lang racket
(require syntax/modread)

(define (list-collection-path path)
  (for/set ([d (in-list (directory-list path #:build? #t))]
            #:when (directory-exists? d))
    (define-values (b f s) (split-path d))
    (cons (path->string f) d)))

(define (list-collection-link link)
  (for/fold ([s (set)])
            ([l (in-list
                 (with-module-reading-parameterization
                   (λ ()
                     (call-with-input-file link read))))])
    (match l
      [(list* (or 'root 'static-root) p (or (list reg) reg))
       (let ([p (simplify-path (build-path link 'up p))])
         (if (and (or (null? reg) (regexp-match? reg (version)))
                  (directory-exists? p))
             (set-union s (list-collection-path p))
             s))]
      [(list* k p (or (list reg) reg))
       (let ([p (simplify-path (build-path link 'up p))])
         (if (and (or (null? reg) (regexp-match? reg (version)))
                  (directory-exists? p))
             (set-add s (cons k p))
             s))])))

(define (collections)
  (define s
    (let loop ([ls (current-library-collection-links)])
      (match ls
        ['() (set)]
        [`(#f . ,r)
         (set-union
          (for/fold ([s (set)])
                    ([p (in-list (current-library-collection-paths))]
                     #:when (directory-exists? p))
            (set-union s (list-collection-path p)))
          (loop r))]
        [(cons (? path? p) r)
         (set-union
          (list-collection-link p)
          (loop r))]
        [(cons (? hash? h) r)
         (for*/fold ([s (loop r)])
                    ([(k v) (in-hash h)]
                     [p (in-list v)])
           (if k
               (set-add s (cons (symbol->string k) p))
               (set-union s (list-collection-path p))))])))
  (for/fold ([h (hash)])
            ([p (in-set s)])
    (hash-update h (car p)
                 (λ (s) (set-add s (cdr p)))
                 (λ () (set (cdr p))))))

(define (get-completions str [cols (collections)])
  (match (string-split str "/" #:trim? #f)
    [(and frags (list col path path* ...))
     (define (join-str f)
       (string-join (append (drop-right frags 1) (list f)) "/"))
     (let ([dirs (hash-ref cols col (λ () #f))])
       (cond
         [(not dirs) (set)]
         [else
          (for/fold ([s (set)])
                    ([dir (in-set dirs)])
            (let loop ([path^ (cons path path*)] [dir dir])
              (match path^
                [(list p)
                 (for/fold ([s s])
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
                 (define more-dir (build-path dir p))
                 (if (directory-exists? more-dir)
                     (loop path* more-dir)
                     s)]
                [else s])))]))]
    [else (set)]))

(provide collections get-completions)