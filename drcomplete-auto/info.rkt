#lang info
(define name "drcomplete-auto")
(define version "0.1")
(define collection "drcomplete-auto")
(define deps (quote ("base" "gui-lib" "drracket-plugin-lib" "drracket")))
(define build-deps (quote ()))

(define drracket-tools '(("tool.rkt")))
(define drracket-tool-names '("drcomplete-auto"))
(define drracket-tool-icons '(#f))