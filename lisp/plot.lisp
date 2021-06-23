(in-package :delta-vega)


(defun make-plot (&key data
&key title id update x-title y-title width height)
  (jupyter:vega-lite
    (make-top-view :data (make-vector-data "x" x "y" y)
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

