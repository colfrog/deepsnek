(load "qlearn.lisp")
(load "vectorized-matrix.lisp")

;;; state: #(direction apple-direction head-up head-left head-right)
;;; action: either of: do nothing (0), turn left (1), turn right(2)

(defclass snake-agent (ql-agent)
  ((board
    :initarg :board
    :accessor board
    :initform (make-instance 'board :size 15))
   (board-matrix
    :accessor board-matrix
    :initform nil)
   (epsilon-start
    :initarg :epsilon-start
    :accessor epsilon-start
    :initform 1)
   (epsilon-end
    :initarg :epsilon-end
    :accessor epsilon-end
    :initform 0.1)
   (epsilon-decay
    :initarg :epsilon-decay
    :accessor epsilon-decay
    :initform 0.00001)
   (each-step
    :initarg :each-step
    :accessor each-step
    :initform nil)))

(defmethod make-board-matrix ((b board))
  "Build a matrix representing the game"
  (with-slots (size apple snake) b
    (with-slots (pos) snake
      (let ((matrix (make-vectorized-matrix size size)))
	(dotimes (i size)
	  (dotimes (j size)
	    (cond
	      ((equal (car pos) (cons i j))
	       (vmset matrix i j 1)) ; 1 is the head
	      ((in-snake snake (cons i j))
	       (vmset matrix i j 2)) ; 2 is the body
	      ((equal apple (cons i j))
	       (vmset matrix i j 3))))) ; 3 is the apple
	matrix))))

(defmethod pre-update-board-matrix ((as snake-agent))
  "Update the snake's tail and head position in the board matrix"
  (with-slots (board board-matrix) as
    (with-slots (snake apple) board
      (with-slots (pos growing) snake
	(when (= growing 0)
	  (let ((tail (car (last pos))))
	    (vmset board-matrix (car apple) (cdr apple) 0)
	    (vmset board-matrix (caar pos) (cdar pos) 1)
	    (vmset board-matrix (car tail) (cdr tail) 0)))))))

(defmethod post-update-board-matrix ((as snake-agent))
  "Update the snake's head and the apple's positions in the board matrix"
  (with-slots (board board-matrix) as
    (with-slots (snake apple) board
      (with-slots (pos) snake
	(vmset board-matrix (caar pos) (cdar pos) 2)
	(vmset board-matrix (car apple) (cdr apple) 3)))))

(defmethod around-snake ((b board))
  "Returns whether the snake's immediate surroundings will kill it"
  (with-slots (snake size) b
    (with-slots (dir pos) snake
      (let* ((ahead (make-dir-modifier dir))
	     (left (cons (cdr ahead) (- (car ahead))))
	     (right (cons (- (cdr ahead)) (car ahead))))
	(list (lose-conditions snake (add-points ahead (car pos)) size)
	      (lose-conditions snake (add-points left (car pos)) size)
	      (lose-conditions snake (add-points right (car pos)) size))))))

(defmethod get-apple-dir ((b board))
  "Returns the direction of the apple relative to the snake's head"
  (with-slots (snake apple) b
    (with-slots (pos) snake
      (let ((head (car pos)))
	(cond
	  ((and (< (car head) (car apple))
		(= (cdr head) (cdr apple)))
	   *dir-left*)
	  ((and (> (car head) (car apple))
		(= (cdr head) (cdr apple)))
	   *dir-right*)
	  ((and (= (car head) (car apple))
		(< (cdr head) (cdr apple)))
	   *dir-up*)
	  ((and (= (car head) (car apple))
		(> (cdr head) (cdr apple)))
	   *dir-down*)
	  (t *dir-none*))))))

(defmethod get-safe-dir ((b board))
  "Returns the first safe direction that the snake can start with"
  (with-slots (snake size) b
    (with-slots (pos) snake
      (let ((directions #((0 . 0) (0 . -1) (0 . 1) (-1 . 0) (1 . 0))))
	(dotimes (i (length directions))
	  (when (not (lose-conditions snake (add-points (car pos) (aref directions i)) size))
	    (return i)))))))

(defmethod turn-snake ((b board) action)
  "Turns the snake left if action is 1 or right if action is 2"
  (when (> action 0)
    (with-slots (snake) b
      (with-slots (dir) snake
	(let* ((dirmap (vector (cons *dir-left* *dir-right*) ; up
			      (cons *dir-right* *dir-left*) ; down
			      (cons *dir-down* *dir-up*) ; left
			      (cons *dir-up* *dir-down*))) ;right
	      (dirpair (aref dirmap (- dir 1))))
	  (if (= action 1)
	      (change-dir b (car dirpair)) ; turn left
	      (change-dir b (cdr dirpair)))))))) ; turn right

(defmethod make-state ((sa snake-agent))
  (with-slots (board) sa
    (with-slots (snake) board
      (with-slots (dir) snake
	(append (list dir (get-apple-dir board))
		(around-snake board))))))

(defmethod update-epsilon ((sa snake-agent) current-step)
  "Updates epsilon using exponential decay"
  (with-slots (epsilon epsilon-start epsilon-end epsilon-decay) a
    (setf epsilon
	  (+ epsilon-end
	     (* (- epsilon-start epsilon-end)
		(exp (- (* current-step epsilon-decay))))))))

(defmethod stepfun ((sa snake-agent) action)
  "An iteration of the game, the AI expects to be returned to with (next-state . reward)"
  (with-slots (board each-step) sa
    (with-slots (apple-eaten game-over) board
      (when each-step
	(funcall each-step))
      
      (turn-snake board action)
      (pre-update-board-matrix sa)
      (update-game board)
      (post-update-board-matrix sa)
      (let* ((reward (+ (if game-over (* game-over 100) 0)
			(if apple-eaten 100 0))))
	(cons
	 (when (not game-over)
	   (make-state sa))
	 reward)))))

(defun make-snake-agent (&key)
  "Creates the Q Learning agent for snake"
  (let ((sa (make-instance
	     'snake-agent
	     :epsilon 1
	     :discount-factor 0.1
	     :learning-rate 1
	     :nstates 5
	     :nactions 3
	     :hidden-layers '(32 32)
	     :sample-batch-size 32
	     :memory-len 500
	     :steps-to-target-network-update 500)))
    (with-slots (board) sa
      (init-agent sa)
      (init-game board)
      sa)))

(defmethod init-game ((sa snake-agent))
  "Initializes the game tied to sa"
  (with-slots (board board-matrix) sa
    (init-game board)
    (setf board-matrix (make-board-matrix board))
    (change-dir board (get-safe-dir board))))

(defmethod run-ai ((sa snake-agent) &key (count 1))
  "Runs `count` iterations of the game to be played by the AI in `sa`"
  (with-slots (board) sa
    (dotimes (i count)
      (init-game sa)
      (run-episode sa (make-state sa)))))
