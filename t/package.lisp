(defpackage :shlex/test
  (:use :cl :alexandria :serapeum :fiveam)
  (:import-from :shlex
    :make-lexer :lexer-tokens)
  (:shadow :test :quote)
  (:export :run-all-tests))
