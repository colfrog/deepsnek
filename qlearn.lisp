(defclass agent ()
  ((stepfun
    :initarg :stepfun
    :accessor stepfun
    :initform nil)
   (epsilon
    :initarg :epsilon
    :accessor epsilon
    :initform 0)
   (learning-rate
    :initarg :learning-rate
    :accessor learning-rate
    :initform 1)
   (discount-factor
    :initarg :discount-factor
    :accessor discount-factor
    :initform 0.1)
   (actions
    :initarg :actions
    :accessor actions
    :initform nil)
   (qtable
    :accessor qtable
    :initform nil)))

(defmethod init-agent ((a agent) &key (nstates 100))
  "Initializes the agent"
  (with-slots (qtable) a
    (setf qtable (make-hash-table :test 'equal :size nstates))))

(defmethod set-action-value ((a agent) state action-index new-action-value)
  "Sets the action value for `state` for the action in `action-index` to `new-action-value`"
  (with-slots (qtable actions) a
    (let* ((action-values (gethash state qtable (make-array (list (length actions))
							    :initial-element 0))))
      (setf (aref action-values action-index) new-action-value)
      (setf (gethash state qtable) action-values))))

(defmethod explore-action ((a agent) state)
  "Returns a random action's index"
  (with-slots (actions) a
    (random (length actions))))

(defmethod exploit-action ((a agent) state)
  "Returns the action's index for the action with the maximum value for `state`"
  (with-slots (qtable actions) a
    (let ((action-values (gethash state qtable
				  (make-array (list (length actions))
					      :initial-element 0))))
      (if (every #'zerop action-values)
	  (explore-action a state)
	  (position (reduce #'max action-values) action-values)))))
    
(defmethod get-action ((a agent) state)
  "Returns an action decided by exploration or exploitation according to epsilon"
  (with-slots (epsilon) a
    (if (< (/ (random 100) 100) epsilon)
	(explore-action a state)
	(exploit-action a state))))

(defmethod new-action-value ((a agent) state next-state action-index reward)
  "Calculates the new action value according to the reward and the agent's constants"
  (with-slots (qtable learning-rate discount-factor actions) a
    (let* ((action-values (gethash state qtable (make-array (list (length actions))
							    :initial-element 0)))
	  (action-value (aref action-values action-index))
	  (next-action-values (gethash next-state qtable (make-array (list (length actions))
								     :initial-element 0)))
	  (next-max-value (reduce #'max next-action-values)))
      (+ (* (- 1 learning-rate) action-value)
	 (* learning-rate (+ reward (* discount-factor next-max-value)))))))

(defmethod run-episode ((a agent) initial-state)
  "Runs an episode: recalculate everything and decide on an action until stepfun returns a nil next-step"
  (with-slots (stepfun actions) a
    (do* ((state initial-state next-state)
	  (action-index (get-action a state) (get-action a state))
	  (action (aref actions action-index) (aref actions action-index))
	  (step-results (funcall stepfun a action) (funcall stepfun a action))
	  (next-state (car step-results) (car step-results))
	  (reward (cdr step-results) (cdr step-results))
	  (action-value (new-action-value a state next-state action-index reward)
			  (new-action-value a state next-state action-index reward)))
	 ((not state) nil)
      (set-action-value a state action-index action-value))))
