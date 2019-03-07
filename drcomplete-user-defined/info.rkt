#lang info
(define collection "drcomplete-user-defined")
(define deps '("base" "drracket" "drracket-plugin-lib" "gui-lib" "syntax-color-lib"))
(define build-deps '("rackunit-lib"))
(define pkg-desc "auto complete for user defined identifiers")
(define version "0.1")
(define pkg-authors '(yjqww6))


(define drracket-tools '(("tool.rkt")))
(define drracket-tool-names '("drcomplete-user-defined"))
(define drracket-tool-icons '(#f))
