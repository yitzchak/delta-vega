(in-package :delta-vega)


(defun scatter-plot (x y &key title id update x-title y-title regression)
  (jupyter:vega-lite `(:object-plist "$schema" "https://vega.github.io/schema/vega-lite/v5.json"
                                     ,@(when title
                                         (list "title" title))
                                     "data" (:object-plist "values" ,(map 'vector
                                                                          (lambda (x y)
                                                                            (list :object-plist
                                                                                  "x" x
                                                                                  "y" y))
                                                                          x y))
                                     "width" 500
                                     "height" 400
                                     "layer" ((:object-plist "mark" "point"
                                                             "encoding" (:object-plist "x" (:object-plist "field" "x"
                                                                                                          "type" "quantitative"
                                                                                                          ,@(when x-title
                                                                                                              (list "axis" (list :object-plist "title" x-title))))
                                                                                       "y" (:object-plist "field" "y"
                                                                                                          "type" "quantitative"
                                                                                                          ,@(when x-title
                                                                                                              (list "axis" (list :object-plist "title" y-title))))))
                                              ,(when regression
                                                  `(:object-plist "mark" (:object-plist "type" "line"
                                                                                       "color" "black")
                                                                  "transform" ((:object-plist "regression" "y"
                                                                                              "on" "x"))
                                                                  "encoding" (:object-plist "x" (:object-plist "field" "x"
                                                                                                               "type" "quantitative")
                                                                                            "y" (:object-plist "field" "y"
                                                                                                               "type" "quantitative"))))))
                     :display t
                     :id id
                     :update update))
