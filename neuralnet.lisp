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
    :initform 0)
   (errors
    :accessor errors
    :initform (make-hash-table :test 'equal))))

(defun sigmoid (x)
  (/ 1 (+ 1 (exp (- x)))))

(defun sigmoid-derivative (x)
  (* (sigmoid x) (- 1 (sigmoid x))))

(defun random-weight ()
  (- (/ (random 2000) 1000) 1))

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

(defmethod copy-neuron ((to-neu neuron) (from-neu neuron))
  (with-slots ((to-links links)) to-neu
    (with-slots ((from-links links)) from-neu
      (do* ((to-links-tail to-links (cdr to-links-tail))
	    (from-links-tail from-links (cdr from-links-tail))
	    (to-link (car to-links) (car to-links-tail))
	    (from-link (car from-links) (car from-links-tail)))
	   ((not from-links-tail) nil)
	(setf (cdr to-link) (cdr from-link))))))

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

(defmethod get-neuron ((lay layer) (index number))
  (with-slots (neurons) lay
    (do* ((neus neurons (cdr neus))
	  (neu (car neus) (car neus))
	  (i 0 (1+ i)))
	 ((or (> i (length neurons)) (not neus)) nil)
      (when (= i index)
	(return neu)))))

(defmethod copy-layer ((to-lay layer) (from-lay layer))
  (with-slots ((to-neurons neurons)) to-lay
    (with-slots ((from-neurons neurons)) from-lay
      (do* ((to-neurons-tail to-neurons (cdr to-neurons-tail))
	    (from-neurons-tail from-neurons (cdr from-neurons-tail))
	    (to-neuron (car to-neurons) (car to-neurons-tail))
	    (from-neuron (car from-neurons) (car from-neurons-tail)))
	   ((not from-neurons-tail) nil)
	(copy-neuron to-neuron from-neuron)))))

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

(defmethod run-network ((net network) (input vector))
  (with-slots (layers) net
    (set-layer (car layers) input)
    (eval-network net)
    (get-layer-values (car (last layers)))))

(defmethod run-network ((net network) (input-list cons))
  (let ((output-list nil))
    (dolist (input input-list (reverse output-list))
      (setf output-list (cons (run-network net input) output-list)))))

(defmethod backpropagate ((net network) (neu neuron) (loss number))
  (with-slots (errors) net
    (with-slots (links value) neu
      (when links
	(let ((err (* loss (sigmoid-derivative value))))
	  (dolist (link links)
	    (backpropagate net (car link) (* (cdr link) err))
	    (setf (gethash link errors)
		  (cons (* (get-neuron-value (car link)) err)
			(gethash link errors nil)))))))))

(defun update-weight (link errvals)
  (setf (cdr link)
	(- (cdr link)
	   (/ (reduce #'+ errvals) (length errvals)))))

(defmethod update-weights ((net network))
  (with-slots (errors) net
    (maphash #'update-weight errors)
    (setf errors (make-hash-table :test 'equal))))

(defmethod teach-neural-network ((net network) (in vector) (out number) (output-node number)
			  &key (iterations 100) (update t))
  (with-slots (layers) net
    (let ((output-neuron (get-neuron (car (last layers)) output-node)))
      (dotimes (i iterations)
	(run-network net in)
	(let ((loss (- (get-neuron-value output-neuron) out)))
	  (backpropagate net output-neuron loss)
	  (when update
	    (update-weights net)))))))

(defmethod teach-neural-network ((net network) (inlist cons) (outlist cons) (output-nodes cons)
				&key (iterations 100))
  (dotimes (i iterations)
    (do* ((inlist-tail inlist (cdr inlist-tail))
	  (outlist-tail outlist (cdr outlist-tail))
	  (output-nodes-tail output-nodes (cdr output-nodes-tail))
	  (in (car inlist) (car inlist-tail))
	  (out (car outlist) (car outlist-tail))
	  (output-node (car output-nodes) (car output-nodes)))
	 ((not (and inlist-tail outlist-tail)) nil)
      (teach-neural-network net in out output-node :iterations 1 :update nil))
    (update-weights net)))

(defun make-network (in hidden out)
  (let ((net (make-instance 'network :output-nodes out :input-nodes in :hidden-layers hidden)))
    (init-network net)
    net))

(defmethod clone-network ((net network))
  (with-slots (input-nodes hidden-layers output-nodes (from-lays layers)) net
    (let ((clone (make-network input-nodes hidden-layers output-nodes)))
      (with-slots ((to-lays layers)) clone
	(do* ((from-lays-tail (cdr from-lays) (cdr from-lays-tail))
	      (to-lays-tail (cdr to-lays) (cdr to-lays-tail))
	      (from-lay (car from-lays-tail) (car from-lays-tail))
	      (to-lay (car to-lays-tail) (car to-lays-tail)))
	     ((not from-lays-tail) clone)
	  (copy-layer to-lay from-lay))))))
