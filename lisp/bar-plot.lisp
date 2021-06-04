(in-package :delta-vega)


(defun bar-plot (x y &key title id update x-title y-title regression)
  (jupyter:vega-lite
    `(:object-plist
      "$schema" "https://vega.github.io/schema/vega-lite/v5.json"
      ,@(when title
        (list "title" title))
      "width" 500
      "height" 400
      "data" (:object-plist
              "values" ,(map 'vector
                             (lambda (x y)
                               (list :object-plist
                                     "x" x
                                     "y" y))
                             x y))
      "mark" "bar"
      "encoding" (:object-plist
                  "x" (:object-plist
                       "field" "x"
                       "type" "ordinal"
                       ,@(when x-title
                           (list "axis" (list :object-plist "title" x-title))))
                  "y" (:object-plist
                       "field" "y"
                       "type" "quantitative"
                       ,@(when x-title
                         (list "axis" (list :object-plist "title" y-title))))))))

