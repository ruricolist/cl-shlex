(defpackage :shlex
  (:use :cl :alexandria :serapeum)
  (:shadow :quote :whitespace)
  (:export
   :split
   :map-tokens
   :do-tokens
   :quote))
