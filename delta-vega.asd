(asdf:defsystem #:delta-vega
  :description "Wrapper functions for VegaLite in common-lisp-jupyter"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on
    (:common-lisp-jupyter :trivial-do)
  :components
    ((:module lisp
      :serial t
      :components
        ((:file "packages")
         (:file "parametric-plot")
         (:file "scatter-plot")
         (:file "bar-plot"))))
  . #+asdf3
      (:version "0.1"
       :homepage "https://yitzchak.github.io/delta-vega/"
       :bug-tracker "https://github.com/yitzchak/delta-vega/issues")
    #-asdf3 ())

