(load "qlearn.lisp")

;;; state: #(direction apple-direction head-up head-left head-right)
;;; action: either of: turn left (-1), do nothing (0), turn right(1)

(defclass snake-agent (agent)
  ((board
    :initarg :board
    :accessor board
    :initform (make-instance 'board :size 15))
   (board-matrix
    :accessor board-matrix
    :initform nil)
   (each-step
    :initarg :each-step
    :accessor each-step
    :initform nil)))

(defmethod make-board-matrix ((b board))
  "Build a matrix representing the game"
  (with-slots (size apple snake) b
    (with-slots (pos) snake
      (let ((matrix (make-array (list size size) :initial-element 0)))
	(dotimes (i size)
	  (dotimes (j size)
	    (cond
	      ((equal (car pos) (cons i j))
	       (setf (aref matrix i j) 1)) ; 1 is the head
	      ((in-snake snake (cons i j))
	       (setf (aref matrix i j) 2)) ; 2 is the body
	      ((equal apple (cons i j))
	       (setf (aref matrix i j) 3))))) ; 3 is the apple
	matrix))))

(defmethod pre-update-board-matrix ((as snake-agent))
  "Update the snake's tail and head position in the board matrix"
  (with-slots (board board-matrix) as
    (with-slots (snake apple) board
      (with-slots (pos growing) snake
	(when (= growing 0)
	  (let ((tail (car (last pos))))
	    (setf (aref board-matrix (car apple) (cdr apple)) 0)
	    (setf (aref board-matrix (caar pos) (cdar pos)) 1)
	    (setf (aref board-matrix (car tail) (cdr tail)) 0)))))))

(defmethod post-update-board-matrix ((as snake-agent))
  "Update the snake's head and the apple's positions in the board matrix"
  (with-slots (board board-matrix) as
    (with-slots (snake apple) board
      (with-slots (pos) snake
	(setf (aref board-matrix (caar pos) (cdar pos)) 2)
	(setf (aref board-matrix (car apple) (cdr apple)) 3)))))

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
  "Turns the snake left if action is -1 or right if actions is 1"
  (when (/= action 0)
    (with-slots (snake) b
      (with-slots (dir) snake
	(let* ((dirmap (vector (cons *dir-left* *dir-right*) ; up
			      (cons *dir-right* *dir-left*) ; down
			      (cons *dir-down* *dir-up*) ; left
			      (cons *dir-up* *dir-down*))) ;right
	      (dirpair (aref dirmap (- dir 1))))
	  (if (< action 0)
	      (change-dir b (car dirpair)) ; turn left
	      (change-dir b (cdr dirpair)))))))) ; turn right

(defmethod make-state ((sa snake-agent))
  (with-slots (board) sa
    (with-slots (snake) board
      (with-slots (dir) snake
	(append (list dir (get-apple-dir board))
		(around-snake board))))))

(defmethod snake-step ((sa snake-agent) action)
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
	     :epsilon 0.2
	     :learning-rate 1
	     :discount-factor 0.1
	     :actions #(-1 0 1)
	     :stepfun #'snake-step)))
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
