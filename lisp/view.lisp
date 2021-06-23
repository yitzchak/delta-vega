(in-package :delta-vega)


(defparameter +vega-lite-schema+ "https://vega.github.io/schema/vega-lite/v5.json")


(defclass view (vega-object)
  ((align
     :accessor align
     :initarg :align)
   (autosize
     :accessor autosize
     :initarg :autosize)
   (background
     :accessor background
     :initarg :background)
   (bounds
     :accessor bounds
     :initarg :bounds)
   (center
     :accessor center
     :initarg :center)
   (config
     :accessor config
     :initarg :config)
   (data
     :accessor data
     :initarg :data)
   (description
     :accessor description
     :initarg :description)
   (encoding
     :accessor encoding
     :initarg :encoding)
   (height
     :accessor height
     :initarg :height)
   (layer
     :accessor layer
     :initarg :layer
     :type (or list vector))
   (mark
     :accessor mark
     :initarg :mark)
   (name
     :accessor name
     :initarg :name)
   (padding
     :accessor padding
     :initarg :padding)
   (params
     :accessor params
     :initarg :params
     :type (or list vector))
   (projection
     :accessor projection
     :initarg :projection)
   (resolve
     :accessor resolve
     :initarg :resolve)
   (spacing
     :accessor spacing
     :initarg :spacing)
   (title
     :accessor title
     :initarg :title)
   (transform
     :accessor transform
     :initarg :transform
     :type (or list vector))
   (usermeta
     :accessor usermeta
     :initarg :usermeta)
   (view
     :accessor view
     :initarg :view)
   (width
     :accessor width
     :initarg :width)))


(defun make-view (&rest initargs &key &allow-other-keys)
  (apply #'make-instance 'view initargs))


(defclass top-view (view)
  (($schema
     :accessor $schema
     :initarg :$schema
     :initform +vega-lite-schema+)))


(defun make-top-view (&rest initargs &key &allow-other-keys)
  (apply #'make-instance 'top-view initargs))


