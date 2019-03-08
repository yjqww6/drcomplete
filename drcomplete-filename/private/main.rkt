#lang typed/racket/base

(require racket/string racket/match racket/format racket/port)

(provide get-completions)

(require srfi/2)

(define (escape [str : String])
  (define written (~s str))
  (substring written 1 (- (string-length written) 1)))

(define (unescape [str : String])
  (with-handlers ([exn:fail:read? (λ (_) #f)])
    (cast (read (open-input-string (string-append "\"" str "\"")))
          String)))

(define (parse-path [str : String])
  (define-values (base sub dir?) (split-path str))
  (cond
    [(or dir? (memq sub '(same up))) (values str #f)]
    [(eq? base 'relative) (values "." sub)]
    [(eq? base #f) (values "/" sub)]
    [else (values base sub)]))

(define (get-completions [str : String])
  (and-let*
   ([str (unescape str)])
   (map
    escape
    (cond
      [(string=? str "") (map path->string (directory-list))]
      [else
       (define-values (b s) (parse-path str))
       (cond
         [(not s)
          (map (λ ([f : Path])
                 (string-append
                  (path->string (path->directory-path b))
                  (path->string f)))
               (with-handlers ([exn:fail:filesystem? (λ (e) '())])
                 (directory-list str)))]
         [else
          (map (λ ([f : Path])
                 (path->string (build-path b f)))
               (filter (λ ([f : Path]) (string-prefix? (path->string f) (path->string s)))
                       (with-handlers ([exn:fail:filesystem? (λ (e) '())])
                         (directory-list b))))])]))))
