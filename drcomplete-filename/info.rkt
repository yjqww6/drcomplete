#lang info
(define collection "drcomplete-filename")
(define deps '("base" "drracket" "drracket-plugin-lib" "gui-lib" "srfi-lib"
                      "drcomplete-base"))
(define build-deps '())
(define pkg-desc "auto complete for filenames")
(define version "0.2")
(define pkg-authors '(yjqww6))


(define drracket-tools '(("tool.rkt")))
(define drracket-tool-names '("drcomplete-filename"))
(define drracket-tool-icons '(#f))
