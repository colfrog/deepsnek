(ql:quickload :sdl2)
(load "snake.lisp")

(defparameter *cell-size* 20)
(defparameter *delay-time* 200)
(defparameter *board* (make-instance 'board :size 25))

(defmacro with-initialised-sdl ((window-sym renderer-sym) title window-size &body body)
  `(sdl2:with-init (:everything)
     (sdl2:with-window (,window-sym :title ,title :w ,window-size :h ,window-size :flags '(:shown))
       (sdl2:with-renderer (,renderer-sym ,window-sym :flags '(:accelerated))
	 ,@body))))

(defun draw-point (point renderer r g b a)
  (with-slots (board-matrix) b
    (when point
      (let*
	  ((x (* *cell-size* (car point)))
	   (y (* *cell-size* (cdr point)))
	   (rect (sdl2:make-rect x y *cell-size* *cell-size*)))
	(sdl2:set-render-draw-color renderer r g b a)
	(sdl2:render-draw-rect renderer rect)))))

(defun draw-points (points renderer r g b a)
  (dolist (p points nil)
    (draw-point p renderer r g b a)))

(defun draw-board (size renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 0)
  (sdl2:render-fill-rect renderer (sdl2:make-rect 0 0 size size)))

(defun draw-snake (b renderer)
  (draw-points (get-snake b) renderer 0 255 0 0))

(defun draw-apple (b renderer)
  (draw-point (get-apple b) renderer 255 0 0 0))

(defun draw-game (window-size b renderer)
  (draw-board window-size renderer)
  (draw-apple b renderer)
  (draw-snake b renderer))

(defun verify-game-end (b)
  (cond ((is-lost b) -1)
	((is-won b) 1)
	(t 0)))

(defun handle-keypress (b scancode)
  (cond
    ((sdl2:scancode= scancode :scancode-q)
     (sdl2:push-event :quit))
    ((or (sdl2:scancode= scancode :scancode-up)
	 (sdl2:scancode= scancode :scancode-w))
     (change-dir b *up*))
    ((or (sdl2:scancode= scancode :scancode-down)
	 (sdl2:scancode= scancode :scancode-s))
     (change-dir b *down*))
    ((or (sdl2:scancode= scancode :scancode-left)
	 (sdl2:scancode= scancode :scancode-a))
     (change-dir b *left*))
    ((or (sdl2:scancode= scancode :scancode-right)
	 (sdl2:scancode= scancode :scancode-d))
     (change-dir b *right*))))

(defun step-game (window-size b renderer)
  (draw-game window-size b renderer)
  (sdl2:render-present renderer))

(defun game-loop ()
  (do () (nil nil)
    (update-game *board*)
    (sdl2:delay *delay-time*)))

(defun start-game (board-size &key (delay-time 200))
  (let ((b (make-instance 'board :size board-size))
	(window-size (* board-size *cell-size*)))
    (init-board b)
    (setf *delay-time* delay-time)
    (setf *board* b)
    (bt:make-thread #'game-loop)
    (with-initialised-sdl (win renderer) "snek" window-size
      (sdl2:with-event-loop (:method :poll)
	(:keydown
	 (:keysym keysym)
	 (let ((scancode (sdl2:scancode-value keysym)))
	   (when (handle-keypress b scancode)
	     (update-game b)
	     (step-game window-size b renderer))))
	(:idle
	 ()
	 (when (/= 0 (verify-game-end b))
	   (sdl2:push-event :quit))
	 (sdl2:delay 10)
	 (step-game window-size b renderer))
	(:quit () t)))))
