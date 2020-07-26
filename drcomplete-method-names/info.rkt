#lang info
(define collection "drcomplete-method-names")
(define deps '("base" "drracket-plugin-lib" "gui-lib"
                      "drcomplete-base"))
(define build-deps '("rackunit-lib"))
(define pkg-desc "auto complete for method names")
(define version "0.2")
(define pkg-authors '(yjqww6))


(define drracket-tools '(("tool.rkt")))
(define drracket-tool-names '("drcomplete-method-names"))
(define drracket-tool-icons '(#f))
