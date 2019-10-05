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
    (with-slots (pos) snake
      (setf pos (list (make-random-point size))))))

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

(defmethod get-snake ((b board))
  (with-slots (snake) b
    (with-slots (pos) snake
      pos)))

(defmethod get-apple ((b board))
  (slot-value b 'apple))

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
    (when (/= dir -1)
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
	   (member npos (cdr pos) :test #'equal)
	   (< (car npos) 0)
	   (>= (car npos) size)
	   (< (cdr npos) 0)
	   (>= (cdr npos) size)))))))

(defmethod is-won ((b board))
  (with-slots (snake) b
    (with-slots (pos) snake
      (>= (length pos) *winning-score*))))

(defmethod iterate-snake ((s snek))
  (with-slots (pos dir growing) s
    (let
	((npos (next-pos s)))
      (when npos
	(progn
	  (setf
	   pos
	   (cons
	    npos
	    (if (> growing 0)
		(progn (setf growing (1- growing)) pos)
		(butlast pos)))))))))

(defmethod maybe-eat-apel ((b board))
  (with-slots (apple snake) b  
    (with-slots (pos growing) snake
      (when (equal (car pos) apple)
	(setf growing (1+ growing))
	(setf apple nil)))))

(defmethod update-game ((b board))
  (with-slots (apple snake) b
    (with-slots (pos) snake
      (maybe-eat-apel b)
      (when (not apple)
	(place-apple b))
      (if (is-lost b)
	  nil
	  (iterate-snake snake)))))

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
	   (= dir -1)
	   (and (or (= dir *up*) (= dir *down*))
		(or (= new-dir *left*) (= new-dir *right*)))
	   (and (or (= dir *left*) (= dir *right*))
		(or (= new-dir *up*) (= new-dir *down*))))
	(setf dir new-dir)))))
