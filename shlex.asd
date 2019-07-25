(defsystem "shlex"
  :description "Lexical analyzer for simple shell-like syntax."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :in-order-to ((test-op (test-op "shlex/test")))
  :depends-on ("alexandria" "serapeum" "cl-ppcre" "cl-unicode")
  :serial t
  :components ((:file "package")
               (:file "shlex")))

(defsystem "shlex/test"
  :description "Test suite for shlex."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on ("shlex" "fiveam")
  :perform (test-op (o c) (symbol-call :shlex/test :run-shlex-tests))
  :pathname "t/"
  :serial t
  :components ((:file "package")
               (:file "test")))
