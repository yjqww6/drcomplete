#lang racket

(provide (all-defined-out))

(define get-dir<%>
  (interface () drcomplete:get-dir drcomplete:path-completions))