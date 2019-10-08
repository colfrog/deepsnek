(defclass vectorized-matrix ()
  ((width
    :initarg :width
    :accessor width
    :initform 0)
   (height
    :initarg :height
    :accessor height
    :initform 0)
   (vec
    :accessor vec
    :initform nil)))

(defun make-vectorized-matrix (width height &key (initial-element 0))
  (let ((m (make-instance 'vectorized-matrix :width width :height height)))
    (with-slots (vec) m
      (setf vec (make-array (list (* width height))
			  :initial-element initial-element))
      m)))

(defmethod get-vector ((m vectorized-matrix))
  (slot-value m 'vec))

(defmethod get-1d-index ((m vectorized-matrix) (i number) (j number))
  (with-slots (height) m
    (+ (* i height) j)))

(defmethod vmref ((m vectorized-matrix) (i number) (j number))
  (with-slots (vec) m
    (aref vec (get-1d-index m i j))))

(defmethod vmset ((m vectorized-matrix) (i number) (j number) elem)
  (with-slots (vec) m
    (setf (aref vec (get-1d-index m i j)) elem)))
