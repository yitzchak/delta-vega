(defpackage #:delta-vega
  (:use #:common-lisp)
  (:import-from :shasht #:make-object)
  (:nicknames :dv)
  (:documentation "Wrapper functions for VegaLite in common-lisp-jupyter")
  (:export
    #:parametric-plot
    #:scatter-plot))
