#lang racket

(provide (all-defined-out))

(define get-dir-key (generate-member-key))
(define check-path-key (generate-member-key))


(define-member-name get-dir get-dir-key)
(define-member-name check-path check-path-key)

(define get-dir<%>
  (interface () get-dir check-path))