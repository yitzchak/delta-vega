(in-package :delta-vega)


(defun scatter-plot (x y &rest parameters &key description title width height id update x-title y-title regression)
  (jupyter:vega-lite
    (make-top-view :data (make-data :values (map 'vector
                                          (lambda (x y)
                                            (list :object-plist
                                                  "x" x
                                                  "y" y))
                                          x y))
               :layer (cons (make-view :mark :point
                                       :encoding (make-encoding :x (make-field-definition :field "x" :type :quantitative
                                                                                          :title x-title)
                                                                :y (make-field-definition :field "y" :type :quantitative
                                                                                          :title y-title)))
                            (when regression
                              (list (make-view :mark :line
                                               :transform (list (make-regression :regression "y" :on "x"))
                                               :encoding (make-encoding :x (make-field-definition :field "x" :type :quantitative)
                                                                        :y (make-field-definition :field "y" :type :quantitative))))))
               :title title
               :width width
               :height height)
    :display t
    :id id
    :update update))

