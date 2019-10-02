(ql:quickload :sdl2)
(load "snake.lisp")

(defparameter *cell-size* 10)

(defmacro with-initialised-sdl ((window-sym renderer-sym) title window-size &body body)
  `(sdl2:with-init (:everything)
     (sdl2:with-window (,window-sym :title ,title :w ,window-size :h ,window-size :flags '(:shown))
       (sdl2:with-renderer (,renderer-sym ,window-sym :flags '(:accelerated))
	 ,@body))))

(defun await-first-input ()
  (sdl2:with-event-loop (:method :poll)
    (:keydown
     (:keysym keysym)
     (when (member (sdl2:scancode-value keysym)
		   '(:scancode-up :scancode-down :scancode-left :scancode-right
		     :scancode-w :scancode-s :scancode a :scancode-d)
		   :test #'sdl2:scancode=)
       (sdl2:push-event :quit)))
    (:idle
     ()
     (sdl2:delay 15))
    (:quit () t)))

(defparameter *colors*
  #(#(100 100 100 255) ; empty
    #(255 0 0 0) ; apple
    #(0 255 0 0))) ; snake
(defun draw-point (b point renderer &key fill)
  (with-slots (board-matrix) b
    (let*
	((x (* *cell-size* (car point)))
	 (y (* *cell-size* (cdr point)))
	 (tile-type (aref board-matrix (car point) (cdr point)))
	 (color (aref *colors* tile-type))
	 (rect (sdl2:make-rect x y *cell-size* *cell-size*)))
      (when fill
	(sdl2:set-render-draw-color renderer 0 0 0 0)
	(sdl2:render-fill-rect renderer rect))
      (sdl2:set-render-draw-color
       renderer
       (aref color 0) (aref color 1) (aref color 2) (aref color 3))
      (sdl2:render-draw-rect renderer rect))))

(defun draw-board (b renderer)
  (with-slots (size) b
    (dotimes (i size nil)
      (dotimes (j size nil)
	(draw-point b (cons i j) renderer :fill t)))))

(defun draw-points (b points renderer)
  (dolist (p points nil)
    (draw-point b p renderer)))

(defun verify-game-end (b)
  (cond ((is-kill (slot-value b 'snake) b) -1)
	((is-won (slot-value b 'snake)) 1)
	(t 0)))

(defun start-game (board-size &key (delay-time 150))
  (let ((b (make-instance 'board :size board-size))
	(board-updated nil))
    (init-board b)

    (with-initialised-sdl (win renderer) "snek" (* board-size *cell-size*)
      (draw-board b renderer)
      (sdl2:with-event-loop (:method :poll)
	(:keydown
	 (:keysym keysym)
	 (let ((scancode (sdl2:scancode-value keysym))
	       (snake (slot-value b 'snake)))
	   (when
	       (cond
		 ((sdl2:scancode= scancode :scancode-q)
		  (sdl2:push-event :quit))
		 ((or (sdl2:scancode= scancode :scancode-up)
		      (sdl2:scancode= scancode :scancode-w))
		  (change-dir snake *up*))
		 ((or (sdl2:scancode= scancode :scancode-down)
		      (sdl2:scancode= scancode :scancode-s))
		  (change-dir snake *down*))
		 ((or (sdl2:scancode= scancode :scancode-left)
		      (sdl2:scancode= scancode :scancode-a))
		  (change-dir snake *left*))
		 ((or (sdl2:scancode= scancode :scancode-right)
		      (sdl2:scancode= scancode :scancode-d))
		  (change-dir snake *right*)))

	     (update-board b)
	     (draw-board b renderer)
	     (sdl2:render-present renderer))))
	(:idle
	 ()
	 (setf board-updated nil)
	 (when (/= 0 (verify-game-end b))
	   (sdl2:push-event :quit))

	 (sdl2:delay delay-time)
	 
	 (update-board b)
	 (draw-board b renderer)
	 (sdl2:render-present renderer))
	(:quit () t)))))
