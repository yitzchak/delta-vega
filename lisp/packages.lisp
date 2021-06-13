(defpackage #:delta-vega
  (:use #:common-lisp)
  (:import-from :shasht #:make-object)
  (:nicknames :dv)
  (:shadow "FORMAT" "METHOD" "TYPE" "VALUES")
  (:documentation "Wrapper functions for VegaLite in common-lisp-jupyter")
  (:export
    #:parametric-plot
    #:make-view
    #:make-field-definition
    #:make-data
    #:make-regression
    #:make-encoding
    #:make-top-view
    #:scatter-plot
    #:bar-plot))
