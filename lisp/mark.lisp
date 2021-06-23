(in-package :delta-vega)


(defclass mark (vega-object)
  ((type
     :accessor type
     :initarg :type
     ;:allocation :class
     :type string)
   (aria
     :accessor aria
     :initarg :aria
     :type (or boolean expr-ref))
   (description
     :accessor description
     :initarg :description
     :type (or string expr-ref))
   (style
     :accessor style
     :initarg :style
     :type (or string list (vector string *)))
   (tooltip
     :accessor tooltip
     :initarg :tooltip
     :type (or number string boolean expr-ref :null))
   (clip
     :accessor clip
     :initarg :clip
     :type boolean)
   (invalid
     :accessor invalid
     :initarg :invalid
     :type (or string null))
   (order
     :accessor order
     :initarg :order
     :type boolean)))


(defclass arc-mark (mark)
  ((radius
    :accessor radius
    :initarg :radius
    :type number-or-expr-ref)
   (radius2
    :accessor radius2
    :initarg :radius2
    :type number-or-expr-ref)
   (inner-radius
    :accessor inner-radius
    :initarg :inner-radius
    :type number-or-expr-ref)
   (outer-radius
    :accessor outer-radius
    :initarg :outer-radius
    :type number-or-expr-ref))
  (:default-initargs
    :type "arc"))


(defun make-arc-mark (&rest initarg &key &allow-other-keys)
  (apply #'make-instance 'arc-mark initarg))


(defclass bar-mark (mark)
  ((width
    :accessor width
    :initarg :width
    :type (or number expr-ref)))
  (:default-initargs
    :type "bar"))


(defun make-bar-mark (&rest initarg &key &allow-other-keys)
  (apply #'make-instance 'bar-mark initarg))


(defclass point-mark (mark)
  ()
  (:default-initargs
    :type "point"))


(defun make-point-mark (&rest initarg &key &allow-other-keys)
  (apply #'make-instance 'point-mark initarg))


(defclass text-mark (mark)
  ((radius
    :accessor radius
    :initarg :radius
    :type number-or-expr-ref))
  (:default-initargs
    :type "text"))


(defun make-text-mark (&rest initarg &key &allow-other-keys)
  (apply #'make-instance 'text-mark initarg))


(defclass tick-mark (mark)
  ()
  (:default-initargs
    :type "tick"))


(defun make-tick-mark (&rest initarg &key &allow-other-keys)
  (apply #'make-instance 'tick-mark initarg))



