(in-package :delta-vega)


(defun random-samples (min max count)
  (loop with range = (/ (- max min) (1- count))
        for i below count
        for pos from min by range
        collect (if (or (zerop 0)
                        (= i (1- count)))
                  pos
                  (+ pos (- (random (/ range 2)) (/ range 4))))))


(defstruct parametric-interval
  min
  max
  x-min
  x-max
  y-min
  y-max
  remaining
  (left :closed)
  (right :closed))


(defun random-intervals (x y min max count remaining)
  (do* ((range (/ (- max min) (1- count)))
        (i 1 (1+ i))
        (epsilon (/ range 4))
        (offset (* range 7/8))
        (right (+ min offset) (+ right range))
        (int (make-parametric-interval :min min
                                       :max (+ right (random epsilon))
                                       :x-min (funcall x min)
                                       :y-min (funcall y min)
                                       :remaining remaining)
             (make-parametric-interval :min (parametric-interval-max int)
                                       :max (if (= i (1- count))
                                              max
                                              (+ right (random epsilon)))
                                       :x-min (parametric-interval-x-max int)
                                       :y-min (parametric-interval-y-max int)
                                       :remaining remaining))
        results)
      ((= i count) (nreverse results))
    (setf (parametric-interval-x-max int) (funcall x (parametric-interval-max int))
          (parametric-interval-y-max int) (funcall y (parametric-interval-max int)))
    (push int results)))


(defun split-p (x-min x-center x-max y-min y-center y-max)
  "This function calculates the curvature of a quadratic polynomial fit to the three points and
   multiplies that by the half distance between the endpoints. Since curvature has units of 1/length
   the result is dimension-less quantity. On a circle in which the endpoints are separated by a
   small angle like a tenth of radian this results in a value of about 0.025"
  (< 0.01 (abs (/ (- (* (- x-max x-min) (- (+ y-min y-max) (* 2 y-center)))
                      (* (- y-max y-min) (- (+ x-min x-max) (* 2 x-center))))
                   (+ (expt (- x-max x-min) 2) (expt (- y-max y-min) 2))))))


(defun analyze-parametric (x y min max samples max-split)
  (prog ((intervals (random-intervals x y min max samples max-split))
         results int center x-center y-center)
   repeat
    (when (null intervals)
      (return (nreverse (cons (list (parametric-interval-max (car results))
                          (parametric-interval-x-max (car results))
                          (parametric-interval-y-max (car results)))
                    (mapcar (lambda (int)
                              (list (parametric-interval-min int)
                                    (parametric-interval-x-min int)
                                    (parametric-interval-y-min int)))
                            results)))))
    (setf int (pop intervals)
          center (+ (random (/ (- (parametric-interval-max int) (parametric-interval-min int)) 4))
                    (* 0.625 (parametric-interval-min int))
                    (* 0.375 (parametric-interval-max int)))
          x-center (funcall x center)
          y-center (funcall y center))
    (cond
      ((and (not (zerop (parametric-interval-remaining int)))
            (split-p (parametric-interval-x-min int)
                     x-center
                     (parametric-interval-x-max int)
                     (parametric-interval-y-min int)
                     y-center
                     (parametric-interval-y-max int)))
        (push (make-parametric-interval :min center
                                        :max (parametric-interval-max int)
                                        :x-min x-center
                                        :x-max (parametric-interval-x-max int)
                                        :y-min y-center
                                        :y-max (parametric-interval-y-max int)
                                        :remaining (1- (parametric-interval-remaining int)))
              intervals)
        (push (make-parametric-interval :min (parametric-interval-min int)
                                        :max center
                                        :x-min (parametric-interval-x-min int)
                                        :x-max x-center
                                        :y-min (parametric-interval-y-min int)
                                        :y-max y-center
                                        :remaining (1- (parametric-interval-remaining int)))
              intervals))
      (t
        (push int results)))
    (go repeat)))


(defun parametric-plot (x y &key title id update x-title y-title min max (samples 20) (max-split 4) point)
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
                                         "layer" ((:object-plist "mark" ,(if point "point" "line")
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
