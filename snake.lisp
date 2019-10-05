(defparameter *winning-score* 100)
(defparameter *dir-none* 0)
(defparameter *dir-up* 1)
(defparameter *dir-down* 2)
(defparameter *dir-left* 3)
(defparameter *dir-right* 4)

(defclass snek ()
  ((pos
    :accessor pos
    :initform nil)
   (pos-dir
    :accessor pos-dir
    :initform nil)
   (dir
    :accessor dir
    :initform *dir-none*)
   (growing
    :accessor growing
    :initform 3)))

(defclass board ()
  ((size
    :initarg :size
    :accessor size
    :initform 0)
   (apple
    :accessor apple
    :initform nil)
   (snake
    :accessor snake
    :initform nil)))

(defun make-random-point (size)
  (cons (random (- size 1)) (random (- size 1))))

(defmethod place-snake ((b board))
  (with-slots (size snake) b
    (with-slots (pos pos-dir dir) snake
      (setf pos (list (make-random-point size)))
      (setf pos-dir (list (cons dir *dir-none*))))))

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
      ((= dir *dir-up*)
       (cons 0 -1))
      ((= dir *dir-down*)
       (cons 0 1))
      ((= dir *dir-left*)
       (cons -1 0))
      ((= dir *dir-right*)
       (cons 1 0))))

(defmethod next-pos ((s snek))
  (with-slots (pos dir) s
    (when (/= dir *dir-none*)
      (let
	  ((head (car pos))
	   (modifier (make-dir-modifier dir)))
	(cons (+ (car head) (car modifier))
	      (+ (cdr head) (cdr modifier)))))))

(defmethod is-lost ((b board))
  (with-slots (size snake) b
    (with-slots (pos) snake
      (let ((npos (next-pos snake)))
	(when npos
	  (or ; lose conditions are listed below
	   (member npos (butlast (cdr pos)) :test #'equal)
	   (< (car npos) 0)
	   (>= (car npos) size)
	   (< (cdr npos) 0)
	   (>= (cdr npos) size)))))))

(defmethod is-won ((b board))
  (with-slots (snake) b
    (with-slots (pos) snake
      (>= (length pos) *winning-score*))))

(defun opposite-dir (dir)
  (let ((opposites (vector *dir-none* *dir-down* *dir-up* *dir-right* *dir-left*)))
    (aref opposites dir)))

(defmethod iterate-snake ((s snek))
  (with-slots (growing pos pos-dir dir) s
    (let ((npos (next-pos s))
	  (opdir (opposite-dir dir)))
      (when npos
	(if (> growing 0)
	    (setf growing (1- growing))
	    (progn
	      (setf pos (butlast pos))
	      (setf pos-dir (butlast pos-dir))))
	(setf pos (cons npos pos))
	(setf (cdar pos-dir) dir)
	(setf pos-dir (cons (cons opdir *dir-none*) pos-dir))))))

(defmethod maybe-eat-apel ((b board))
  (with-slots (apple snake) b  
    (with-slots (pos growing) snake
      (when (equal (car pos) apple)
	(setf growing (1+ growing))
	(setf apple nil)))))

(defmethod update-game ((b board))
  (with-slots (apple snake) b
    (with-slots (pos) snake
      (cond
	((is-lost b) -1)
	((is-won b) 1)
	(t
	 (maybe-eat-apel b)
	 (when (not apple)
	   (place-apple b))
	 (iterate-snake snake)
	 nil)))))

(defmethod init-board ((b board))
  (with-slots (size apple snake) b
    (setf snake (make-instance 'snek))
    (place-snake b)
    (update-game b)))

(defmethod change-dir ((b board) (new-dir number))
  (with-slots (snake) b
    (with-slots (dir) snake
      (when
	  (or
	   (= dir *dir-none*)
	   (and (or (= dir *dir-up*) (= dir *dir-down*))
		(or (= new-dir *dir-left*) (= new-dir *dir-right*)))
	   (and (or (= dir *dir-left*) (= dir *dir-right*))
		(or (= new-dir *dir-up*) (= new-dir *dir-down*))))
	(setf dir new-dir)))))

(defmethod get-snake ((b board))
  (with-slots (snake) b
    (with-slots (pos) snake
      pos)))

(defmethod get-snake-dirs ((b board))
  (with-slots (snake) b
    (with-slots (pos-dir) snake
      pos-dir)))

(defmethod get-apple ((b board))
  (slot-value b 'apple))
