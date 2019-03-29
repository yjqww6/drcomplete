#lang racket

(provide (all-defined-out))

(define get-dir-key (generate-member-key))
(define path-completions-key (generate-member-key))


(define-member-name get-dir get-dir-key)
(define-member-name path-completions path-completions-key)

(define get-dir<%>
  (interface () get-dir path-completions))