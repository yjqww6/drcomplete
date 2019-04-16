#lang info
(define collection "drcomplete-module")
(define deps '("base" "drracket" "drracket-plugin-lib" "gui-lib"
                      "drcomplete-base"))
(define build-deps '())
(define pkg-desc "auto complete for modules")
(define version "0.1")
(define pkg-authors '(yjqww6))


(define drracket-tools '(("tool.rkt")))
(define drracket-tool-names '("drcomplete-module"))
(define drracket-tool-icons '(#f))
