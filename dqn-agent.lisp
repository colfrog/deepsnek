(ql:quickload :random-sample)
(load "neuralnet.lisp")

(defclass dqn-agent ()
  ((epsilon
    :initarg :epsilon
    :accessor epsilon
    :initform 0.1)
   (discount-factor
    :initarg :discount-factor
    :accessor discount-factor
    :initform 0.1)
   (learning-rate
    :initarg :learning-rate
    :accessor learning-rate
    :initform 1)
   (nstates
    :initarg :nstates
    :accessor nstates
    :initform 0)
   (nactions
    :initarg :nactions
    :accessor nactions
    :initform 0)
   (hidden-layers
    :initarg :hidden-layers
    :accessor hidden-layers
    :initform '(64 64))
   (sample-batch-size
    :initarg :sample-batch-size
    :accessor sample-batch-size
    :initform 32)
   (memory-len
    :initarg :memory-len
    :accessor memory-len
    :initform 1000)
   (memory
    :accessor memory
    :initform nil)
   (steps-to-target-network-update
    :initarg :steps-to-target-network-update
    :accessor steps-to-target-network-update
    :initform 1000)
   (q-network
    :accessor q-network
    :initform nil)))

(defgeneric stepfun (dqn-agent action)
  (:documentation "Function called every step of an episode"))

(defgeneric update-epsilon (dqn-agent current-step)
  (:documentation "Function called every step of an episode to update the value of epsilon"))

(defmethod init-agent ((a dqn-agent))
  "Initializes the agent"
  (with-slots (nstates nactions hidden-layers q-network) a
    (setf q-network (make-network nstates hidden-layers nactions))))

(defmethod explore-action ((a dqn-agent) state)
  "Returns a random action's index"
  (with-slots (nactions) a
    (random nactions)))

(defmethod get-action-values ((a dqn-agent) state &key (network nil))
  (let ((net (if network network (slot-value a 'q-network))))
    (run-network net state)))

(defmethod get-max-action-value ((a dqn-agent) state &key (network nil))
  "Returns the maximum predicted action value for `state`"
  (reduce #'max (get-action-values a state :network network)))

(defmethod exploit-action ((a dqn-agent) state)
  "Returns the maximum action value's index for `state`"
  (let* ((action-values (get-action-values a state))
	 (action-value (reduce #'max action-values)))
    (position action-value action-values)))
    
(defmethod get-action ((a dqn-agent) state)
  "Returns an action decided by exploration or exploitation according to epsilon"
  (with-slots (epsilon) a
    (if (< (/ (random 100) 100) epsilon)
	(explore-action a state)
	(exploit-action a state))))

(defmethod get-target ((a dqn-agent) state next-state action reward)
  "Calculates the new action value according to the reward and the agent's constants"
  (with-slots (qtable learning-rate discount-factor nactions) a
    (if (not next-state)
	reward
	(let* ((action-values (gethash state qtable (make-array (list nactions)
								:initial-element 0)))
	       (action-value (aref action-values action))
	       (next-action-values (gethash next-state qtable (make-array (list nactions)
									  :initial-element 0)))
	       (next-max-value (reduce #'max next-action-values)))
	  (* learning-rate (+ reward (- (* discount-factor next-max-value) action-value)))))))

(defmethod push-memory ((a dqn-agent) state action next-state reward)
  "Pushes the state, action, next state and reward to memory"
  (with-slots (memory memory-len) a
    (setf memory
	  (cons (list state action next-state reward)
		(if (> (length memory) memory-len)
		    (butlast memory)
		    memory)))))

(defmethod clear-memory ((a dqn-agent))
  "Clears the memory"
  (with-slots (memory) a
    (setf memory nil)))

(defmethod get-memory-sample ((a dqn-agent))
  "Returns a random sample from memory"
  (with-slots (memory sample-batch-size) a
    (if (< (length memory) sample-batch-size)
	memory
	(random-sample:random-sample memory sample-batch-size))))

(defmethod sort-sample ((a dqn-agent) target-network sample)
  (with-slots (discount-factor) a
    (let ((states nil)
	  (actions nil)
	  (targets nil))
      (dolist (experience sample (list states targets actions))
	(let* ((state (car experience))
	       (action (cadr experience))
	       (reward (caddr experience))
	       (next-state (cadddr experience))
	       (next-max-q (get-max-action-value a next-state :network target-network))
	       (target (+ reward (* discount-factor next-max-q))))
	  (setf states (cons state states))
	  (setf actions (cons action actions))
	  (setf targets (cons target targets)))))))

(defmethod teach-network ((a dqn-agent) (target-network network) sample)
  "Use gradient descent to correct the network using the data in `experience`"
  (with-slots (q-network) a
    (let* ((sorted-sample (sort-sample a target-network sample))
	   (states (car sorted-sample))
	   (targets (cadr sorted-sample))
	   (actions (caddr sorted-sample)))
      (teach-neural-network q-network states targets actions))))

(defmethod run-episode ((a dqn-agent) initial-state)
  "Runs an episode: recalculate everything and decide on an action until stepfun returns a nil next-step"
  (with-slots (nactions q-network memory-len steps-to-target-network-update) a
    (do* ((i 0 (1+ i))
	  (target-network (clone-network q-network) target-network)
	  (state initial-state next-state)
	  (action (get-action a state) (get-action a state))
	  (step-results (stepfun a action) (stepfun a action))
	  (next-state (car step-results) (car step-results))
	  (reward (cdr step-results) (cdr step-results))
	  (net-reward reward (+ net-reward reward)))
	 ((not state) (cons net-reward i))
      
      (push-memory a state action next-state reward)
      
      (when (= (mod i steps-to-target-network-update) 0)
	(setf target-network (clone-network q-network)))
      
      (when (> i memory-len)
	(update-epsilon a i)
	(teach-network a q-network (get-memory-sample a))))))
