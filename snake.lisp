(defparameter *winning-score* 100)
(defparameter *up* 0)
(defparameter *down* 1)
(defparameter *left* 2)
(defparameter *right* 3)

(defclass snek ()
  ((pos
    :accessor pos
    :initform nil)
   (dir
    :accessor dir
    :initform -1)
   (growing
    :accessor growing
    :initform 3)))

(defclass board ()
  ((size
    :initarg :size
    :accessor size
    :initform 0)
   (board-matrix
    :accessor board-matrix
    :initform nil)
   (apple
    :accessor apple
    :initform nil)
   (snake
    :accessor snake
    :initform nil)))

(defun make-random-point (size)
  (cons (random (- size 1)) (random (- size 1))))

(defmethod place-snake ((b board) (s snek))
  (with-slots (board-matrix size) b
    (with-slots (pos) s
      (setf pos (list (make-random-point size)))
      (setf (aref board-matrix (caar pos) (cdar pos)) 2))))

(defmethod in-snake ((s snek) p)
  (with-slots (pos) s
    (member p pos :test #'equal)))

(defmethod place-apple ((b board))
  (with-slots (apple size snake) b
    (setf apple
	  (do ((temp-apple
		(make-random-point size)
		(make-random-point size)))
	      ((not (in-snake snake temp-apple)) temp-apple)))))

(defun make-dir-modifier (dir)
    (cond
      ((= dir *up*)
       (cons 0 -1))
      ((= dir *down*)
       (cons 0 1))
      ((= dir *left*)
       (cons -1 0))
      ((= dir *right*)
       (cons 1 0))))

(defmethod next-pos ((s snek))
  (with-slots (pos dir) s
    (if (= dir -1)
	nil
	(let
	    ((head (car pos))
	     (modifier (make-dir-modifier dir)))
	  (cons (+ (car head) (car modifier))
		(+ (cdr head) (cdr modifier)))))))

(defmethod is-kill ((s snek) (b board))
  (with-slots (pos) s
    (with-slots (size) b
      (or ; lose conditions are listed below
       (member (car pos) (cdr pos) :test #'equal)
       (< (caar pos) 0)
       (>= (caar pos) size)
       (< (cdar pos) 0)
       (>= (cdar pos) size)))))))

(defmethod is-won ((s snek))
  (with-slots (pos) s
    (>= (length pos) *winning-score*)))

(defmethod iterate-snake ((s snek))
  (with-slots (pos dir growing) s
    (let
	((npos (next-pos s))
	 (old-tail (car (last pos))))
      (if (not npos)
	  nil
	  (progn
	    (setf pos
		  (cons npos
			(if (> growing 0)
			    (progn (setf growing (1- growing)) pos)
			    (butlast pos))))
	    old-tail)))))

(defmethod maybe-eat-apel ((s snek) (b board))
  (with-slots (pos growing) s
    (with-slots (apple) b  
      (if (not (equal (car pos) apple))
	  nil
	  (let ((old-apple apple))
	    (setf growing (1+ growing))
	    (setf apple nil)
	    old-apple)))))

(defmethod update-board ((b board))
  (with-slots (board-matrix apple snake) b
    (with-slots (pos) snake
      (let
	  ((old-tail (iterate-snake snake))
	   (old-apple (maybe-eat-apel snake b))
	   (changed-points nil))

	(cond (old-apple (push old-apple changed-points)))

	(cond ((not apple)
	       (place-apple b)
	       (setf (aref board-matrix (car apple) (cdr apple)) 1)
	       (push apple changed-points)))
      
	(cond (old-tail
	       (setf (aref board-matrix (car old-tail) (cdr old-tail)) 0)
	       (push old-tail changed-points)))

	(when (not (is-kill snake b))
	  (setf (aref board-matrix (caar pos) (cdar pos)) 2)
	  (push (car pos) changed-points))

	changed-points))))

(defmethod init-board ((b board))
  (with-slots (size board-matrix apple snake) b
    (setf board-matrix (make-array (list size size) :initial-element 0))
    (setf snake (make-instance 'snek))
    (place-snake b snake)
    (update-board b)))

(defmethod change-dir ((s snek) (new-dir number))
  (with-slots (dir) s
    (when
        (or
	 (= dir -1)
	 (and (or (= dir *up*) (= dir *down*))
	      (or (= new-dir *left*) (= new-dir *right*)))
	 (and (or (= dir *left*) (= dir *right*))
	      (or (= new-dir *up*) (= new-dir *down*))))
      (setf dir new-dir))))
