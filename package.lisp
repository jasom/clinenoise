;;;; package.lisp

(defpackage #:clinenoise
  (:use #:cl #:cffi #:alexandria #:split-sequence)
  (:export "LINENOISE" "HISTORY-ADD" "HISTORY-SAVE" "HISTORY-LOAD" "*COMPLETION-FUNCTION*"))
