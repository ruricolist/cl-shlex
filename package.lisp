(defpackage :shlex
  (:use :cl :alexandria :serapeum)
  (:shadow :quote :whitespace)
  (:export
   :split
   :quote))
