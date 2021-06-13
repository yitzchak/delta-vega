(in-package :delta-vega)


(defun symbol-to-camel-case (sym)
  (do* ((name (symbol-name sym))
        (i 0 (1+ i))
        (downcase t)
        (result (make-array (length name) :fill-pointer 0 :element-type 'character)))
       ((= (length name) i) result)
    (cond
      ((eql #\- (char name i))
        (setf downcase nil))
      (t
        (vector-push (if downcase
                       (char-downcase (char name i))
                       (char-upcase (char name i)))
                     result)
        (setf downcase t)))))


(defclass vega-object ()
  ())


(defmethod shasht:print-json-key-value :around ((object vega-object) key value output-stream)
  (let ((shasht:*symbol-name-function* #'symbol-to-camel-case))
    (call-next-method)))


(defclass data (vega-object)
  ((values
     :accessor values
     :initarg :values
     :type (or list vector string))
   (name
     :accessor name
     :initarg :name
     :type string)
   (format
     :accessor format
     :initarg :format
     :type (member :json :csv :tsv :dsv))))


(defun make-data (&rest initarg &key &allow-other-keys)
  (apply #'make-instance 'data initarg))


(defclass vector-data ()
  ((names
     :accessor names
     :initarg :names
     :type (or list vector))
   (values
     :accessor values
     :initarg :values
     :type (or list vector))))


(defun make-vector-data (x y)
  (make-instance 'vector-data :values (list x y) :names (list "x" "y")))


(defmethod shasht:print-json-value ((instance vector-data) output-stream)
  (with-slots (names values) instance
    (shasht:with-json-object output-stream
      (shasht:with-json-key ("values" output-stream)
        (shasht:print-json-delimiter output-stream)
        (shasht:with-json-array output-stream
          (when values
            (dotimes (index (length (elt values 0)))
              (shasht:print-json-delimiter output-stream)
              (shasht:with-json-object output-stream
                (map nil (lambda (name value)
                           (shasht:print-json-key-value nil name (elt value index) output-stream))
                         names
                         values)))))))))
