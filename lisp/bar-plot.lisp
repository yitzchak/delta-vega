(in-package :delta-vega)


(defun bar-plot (x y &key title id update x-title y-title width height)
  (jupyter:vega-lite
    (make-top-view :data (make-vector-data x y)
               :mark :bar
               :encoding (make-encoding :x (make-field-definition :field "x" :type :ordinal
                                                                  :title x-title)
                                        :y (make-field-definition :field "y" :type :quantitative
                                                                  :title y-title))
               :title title
               :width width
               :height height)
    :display t
    :id id
    :update update))
