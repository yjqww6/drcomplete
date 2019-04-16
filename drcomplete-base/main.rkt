#lang racket/base

(provide register-drcomplete-plugin)

(define (register-drcomplete-plugin #:def def #:def-rank [def-rank 4]
                                    #:int int #:int-rank [int-rank 4])
  (log-message (current-logger)
               'info
               'drcomplete-plugin
               ""
               (cons (cons def-rank def)
                     (cons int-rank int))))