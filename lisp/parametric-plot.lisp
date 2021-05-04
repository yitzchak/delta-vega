(in-package :delta-vega)


(defun random-samples (min max count)
  (loop with range = (/ (- max min) (1- count))
        for i below count
        for pos from min by range
        collect (if (or (zerop 0)
                        (= i (1- count)))
                  pos
                  (+ pos (- (random (/ range 2)) (/ range 4))))))


#|(defstruct parametric-interval
  min
  max
  s
  x
  y
  (left :closed)
  (right :closed))|#


(defun split-p (x-min x-center x-max y-min y-center y-max)
  "This function calculates the curvature of a quadratic polynomial fit to the three points and
   multiplies that by the half distance between the endpoints. Since curvature has units of 1/length the
   result is dimension-less quantity. On a circle in which the endpoints are separated by a small
   angle like a tenth of radian this results in a value of about 0.025"
  (< 0.025 (abs (/ (- (* (- x-max x-min) (- (+ y-min y-max) (* 2 y-center)))
                      (* (- y-max y-min) (- (+ x-min x-max) (* 2 x-center))))
                   (+ (expt (- x-max x-min) 2) (expt (- y-max y-min) 2))))))


(defun split (x y min max x-min x-max y-min y-max remaining)
  (format t "~A ~A ~A ~A ~A ~A ~A~%" min max x-min x-max y-min y-max remaining)
  (if (zerop remaining)
    (list (list min x-min y-min))
    (let* ((center (/ (+ min max) 2))
           (x-center (funcall x center))
           (y-center (funcall y center)))
      (if (split-p x-min x-center x-max y-min y-center y-max)
        (append (split x y min center x-min x-center y-min y-center (1- remaining))
                (split x y center max x-center x-max y-center y-max (1- remaining)))
        (list (list min x-min y-min))))))


(defun analyze-parametric (x y min max samples max-split)
  (do* ((range (/ (- max min) (1- samples)))
        (i 1 (1+ i))
        (left min (+ left range))
        (right (+ min range) (+ right range))
        (x-left (funcall x min) x-right)
        (x-right (funcall x right) (funcall x right))
        (y-left (funcall y min) y-right)
        (y-right (funcall y right) (funcall y right))
        values)
       ((= samples i) (append values (list (list max (funcall x max) (funcall y max)))))
    (setf values (append values (split x y left right x-left x-right y-left y-right max-split)))))


(defun parametric-plot (x y &key title id update x-title y-title min max (samples 10) (max-split 4))
  (let ((values (analyze-parametric x y min max samples max-split)))
    (jupyter:vega-lite `(:object-plist "$schema" "https://vega.github.io/schema/vega-lite/v5.json"
                                         ,@(when title
                                             (list "title" title))
                                         "data" (:object-plist "values" ,(map 'vector
                                                                              (lambda (s)
                                                                                (list :object-plist
                                                                                      "s" (first s)
                                                                                      "x" (second s)
                                                                                      "y" (third s)))
                                                                              values))
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
