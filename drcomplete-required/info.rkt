#lang info
(define collection "drcomplete-required")
(define deps '("base" "drracket" "drracket-plugin-lib" "gui-lib" "srfi-lib"))
(define build-deps '("rackunit-lib"))
(define pkg-desc "auto complete for required identifiers")
(define version "0.1")
(define pkg-authors '(yjqww6))


(define drracket-tools '(("tool.rkt")))
(define drracket-tool-names '("drcomplete-required"))
(define drracket-tool-icons '(#f))
