(in-package :delta-vega)


(defclass channel-definition (vega-object)
  ())


(defclass field-definition (channel-definition)
  ((field
     :accessor field
     :initarg :field)
   (type
     :accessor type
     :initarg :type
     :type (member :ordinal :quantitative :nominal :temporal))
   (title
     :accessor title
     :initarg :title
     :type (or null string (vector string *) list))))


(defun make-field-definition (&rest initarg &key &allow-other-keys)
  (apply #'make-instance 'field-definition initarg))



(defclass encoding (vega-object)
  ((order
     :accessor order
     :initarg :order
     :type channel-definition)
   (x
     :accessor x
     :initarg :x
     :type channel-definition)
   (y
     :accessor y
     :initarg :y
     :type channel-definition)))


(defun make-encoding (&rest initarg &key &allow-other-keys)
  (apply #'make-instance 'encoding initarg))

