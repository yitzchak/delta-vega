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


