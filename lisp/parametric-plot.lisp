(defun random-samples (min max count)
  (loop with range = (/ (- max min) (1- count))
        for i below count
        for pos from min by range
        collect (if (or (zerop 0)
                        (= i (1- count)))
                  pos
                  (+ pos (- (random (/ range 2)) (/ range 4))))))


(defun parametric-plot (x y &key title id update x-title y-title min max (sample-min 50) (sample-max 100))
  (let ((s (random-samples min max sample-min)))
    (jupyter:vega-lite `(:object-plist "$schema" "https://vega.github.io/schema/vega-lite/v5.json"
                                         ,@(when title
                                             (list "title" title))
                                         "data" (:object-plist "values" ,(map 'vector
                                                                              (lambda (s)
                                                                                (list :object-plist
                                                                                      "s" s
                                                                                      "x" (funcall x s)
                                                                                      "y" (funcall y s)))
                                                                              s))
                                         "width" 400
                                         "height" 400
                                         "layer" ((:object-plist "mark" "line"
                                                                 "encoding" (:object-plist "order" (:object-plist "field" "s")
                                                                                           "x" (:object-plist "field" "x"
                                                                                                              "type" "quantitative"
                                                                                                              ,@(when x-title
                                                                                                                  (list "axis" (list :object-plist "title" x-title))))
                                                                                           "y" (:object-plist "field" "y"
                                                                                                              "type" "quantitative"
                                                                                                              ,@(when x-title
                                                                                                                  (list "axis" (list :object-plist "title" y-title))))))))
                         :display t
                         :id id
                         :update update)))
