#lang racket

(require racket scribble/xref setup/xref
         scribble/manual-struct scribble/blueboxes)

(define (methods)

  (define xref 
    (load-collections-xref))

  (define entrys (xref-index xref))

  (define cache (make-blueboxes-cache #t))
  
  (define h (make-hash))
  
  (for ([en (in-list entrys)])
    (match en
      [(entry _ _ _ (method-index-desc name from-libs mn t))
       (for ([lib (in-list from-libs)])
         (hash-update! h lib
                       (λ (s) (set-add! s mn) s)
                       (λ () (mutable-set))))]
      [(entry _ _ _ (constructor-index-desc name from-libs t))
       (define strs (fetch-blueboxes-strs t #:blueboxes-cache cache))
       (for ([str (in-list strs)])
         (define ls
           (regexp-match* #px"\\[([^\\[\\s\u00A0]+).*?\\]" str
                          #:match-select cadr))
         (for ([lib (in-list from-libs)])
           (hash-update!
            h lib
            (λ (s) (set-union! s (list->mutable-set (map string->symbol ls))) s)
            (λ () (mutable-set)))))]
      [else (void)]))
      
  (for/hash ([(k v) (in-hash h)])
    (values k (set->list v))))

(provide methods)