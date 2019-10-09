(defclass neuron ()
  ((value
    :accessor value
    :initform 0)
   (links
    :accessor links
    :initform nil)))

(defclass layer ()
  ((neurons
    :accessor neurons
    :initform nil)
   (size
    :initarg :size
    :accessor size
    :initform 0)
   (input-layer
    :accessor input-layer
    :initform nil)))

(defclass network ()
  ((layers
    :accessor layers
    :initform nil)
   (bias
    :accessor bias
    :initform nil)
   (output-nodes
    :initarg :output-nodes
    :accessor output-nodes
    :initform 0)
   (input-nodes
    :initarg :input-nodes
    :accessor input-nodes
    :initform 0)
   (hidden-layers
    :initarg :hidden-layers
    :accessor hidden-layers
    :initform 0)))

(defun sigmoid (x)
  (/ 1 (+ 1 (exp (- x)))))

(defun sigmoid-derivative (x)
  (* (sigmoid x) (- 1 (sigmoid x))))

(defun random-weight ()
  (/ (random 1000) 1000))

(defmethod link-neurons ((to neuron) (from neuron))
  (with-slots (links) to
    (setf links (cons (cons from (random-weight)) links))))

(defmethod set-neuron ((neu neuron) val)
  (setf (slot-value neu 'value) val))

(defmethod get-neuron-value ((neu neuron))
  (slot-value neu 'value))

(defmethod eval-neuron ((neu neuron))
  (with-slots (value links) neu
    (let ((val 0))
      (setf value
	    (sigmoid
	     (dolist (link links val)
	       (setf val
		     (+ val
			(* (slot-value (car link) 'value)
			   (cdr link))))))))))

(defmethod init-neurons ((lay layer))
  (with-slots (size neurons) lay
    (setf neurons nil)
    (dotimes (i size)
      (setf neurons (cons (make-instance 'neuron) neurons)))))

(defmethod link-input-layer ((lay layer) (input layer))
  (with-slots ((to-list neurons)) lay
    (with-slots ((from-list neurons)) input
      (dolist (to to-list)
	(dolist (from from-list)
	  (link-neurons to from))))))

(defmethod set-layer ((lay layer) (vec vector))
  (with-slots (neurons) lay
    (do* ((neus neurons (cdr neus))
	  (neu (car neus) (car neus))
	  (i 0 (1+ i)))
	 ((or (> i (length vec)) (not neus)) nil)
      (set-neuron neu (aref vec i)))))

(defmethod eval-layer ((lay layer))
  (with-slots (neurons) lay
    (dolist (neu neurons)
      (eval-neuron neu))))

(defmethod get-layer-values ((lay layer))
  (with-slots (neurons) lay
    (let ((vals nil))
      (dolist (neu neurons vals)
	(setf vals (cons (get-neuron-value neu) vals))))))

(defun make-layer (size)
  (let ((lay (make-instance 'layer :size size)))
    (init-neurons lay)
    lay))

(defmethod init-network ((net network))
  (with-slots (layers bias input-nodes hidden-layers output-nodes) net
    (setf bias (make-layer 1))
    (set-layer bias #(1))
    (setf layers (cons (make-layer output-nodes) nil))
    (dolist (size (reverse hidden-layers))
      (setf layers (cons (make-layer size) layers)))
    (setf layers (cons (make-layer input-nodes) layers))
    (do* ((prev nil cur)
	  (lay layers (cdr lay))
	  (cur (car lay) (car lay)))
	 ((not lay) nil)
      (when prev
	(link-input-layer cur prev)
	(link-input-layer cur bias)))))

(defmethod eval-network ((net network))
  (with-slots (layers) net
    (dolist (lay (cdr layers))
      (eval-layer lay))))

(defmethod run-network ((net network) (vec vector))
  (with-slots (layers) net
    (set-neurons (car layers) vec)
    (eval-network net)
    (get-layer-values (car (last layers)))))

(defun make-network (in hidden out)
  (let ((net (make-instance 'network :output-nodes out :input-nodes in :hidden-layers hidden)))
    (init-network net)
    net))
