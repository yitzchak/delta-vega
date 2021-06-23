(defpackage #:delta-vega
  (:use #:common-lisp)
  (:import-from :shasht #:make-object)
  (:nicknames :dv)
  (:shadow "FORMAT" "METHOD" "TYPE" "VALUES")
  (:documentation "Wrapper functions for VegaLite in common-lisp-jupyter")
  (:export
    #:bar-plot
    #:make-arc-mark
    #:make-bar-mark
    #:make-data
    #:make-encoding
    #:make-field-definition
    #:make-point-mark
    #:make-regression
    #:make-text-mark
    #:make-tick-mark
    #:make-top-view
    #:make-vector-data
    #:make-view
    #:parametric-plot
    #:scatter-plot))

