(ql:quickload :sdl2)
(load "snake.lisp")
(load "snake-ai.lisp")

(defparameter *cell-size* 20)
(defparameter *delay-time* 200)
(defparameter *board* (make-instance 'board :size 25))

(defmacro with-initialised-sdl ((window-sym renderer-sym) title window-size &body body)
  "Shorterns SDL initialization"
  `(sdl2:with-init (:everything)
     (sdl2:with-window (,window-sym :title ,title :w ,window-size :h ,window-size :flags '(:shown))
       (sdl2:with-renderer (,renderer-sym ,window-sym :flags '(:accelerated))
	 ,@body))))

(defun draw-point (point renderer r g b a)
  "Draws a point (x . y) of colour rgba in the renderer according to *cell-size*"
  (with-slots (board-matrix) b
    (when point
      (let*
	  ((x (* *cell-size* (car point)))
	   (y (* *cell-size* (cdr point)))
	   (rect (sdl2:make-rect x y *cell-size* *cell-size*)))
	(sdl2:set-render-draw-color renderer r g b a)
	(sdl2:render-draw-rect renderer rect)))))

(defun draw-points (points renderer r g b a)
  "Draw a list of points using draw-point, all the same colour"
  (dolist (p points nil)
    (draw-point p renderer r g b a)))

(defun draw-board (size renderer)
  "Draw the board black"
  (sdl2:set-render-draw-color renderer 0 0 0 0)
  (sdl2:render-fill-rect renderer (sdl2:make-rect 0 0 size size)))

(defun make-custom-rect (x y params)
  "Draw a custom rectangle, using x, y offsets and size multiplicators"
  (sdl2:make-rect (+ x (car params))
		  (+ y (cadr params))
		  (caddr params)
		  (cadddr params)))

;; Define rectangles to draw the snake
(let* ((1c *cell-size*) ; 1 cell
       (1hc (/ 1c 2))   ; 1 half cell
       (1qc (/ 1c 4))   ; 1 quarter cell
       (3qc (* 1qc 3))  ; 3 quarter cell
       (mid (list 1qc 1qc 1hc 1hc))
       (up (list 1qc 0 1hc 3qc))
       (down (list 1qc 1qc 1hc 3qc))
       (left (list 0 1qc 3qc 1hc))
       (right (list 1qc 1qc 3qc 1hc)))
  (defparameter *snake-parts* (vector mid up down left right)))
(defun draw-snake (b renderer)
  "Draw the snake according to rules to place the right rectangles according to the snake part's origin and destination"
  (sdl2:set-render-draw-color renderer 0 255 0 0)
  (do ((body (get-snake b) (cdr body))
       (dirs (get-snake-dirs b) (cdr dirs)))
      ((not (and body dirs)) nil)
    (let* ((x (* *cell-size* (caar body)))
	   (y (* *cell-size* (cdar body)))
	   (orig-dir (caar dirs))
	   (dest-dir (cdar dirs))
	   (orig-rect (make-custom-rect x y (aref *snake-parts* orig-dir)))
	   (dest-rect (make-custom-rect x y (aref *snake-parts* dest-dir))))
      (sdl2:render-fill-rect renderer orig-rect)
      (sdl2:render-fill-rect renderer dest-rect))))

(defun draw-apple (b renderer)
  "Draw the apple"
  (let
      ((apple (get-apple b)))
    (when apple
      (let* ((x (* *cell-size* (car apple)))
	    (y (* *cell-size* (cdr apple)))
	    (rect (make-custom-rect x y (aref *snake-parts* 0))))
      (sdl2:set-render-draw-color renderer 255 0 0 0)
      (sdl2:render-fill-rect renderer rect)))))

(defun draw-game (window-size b renderer)
  "Draw all components of the game"
  (draw-board window-size renderer)
  (draw-apple b renderer)
  (draw-snake b renderer))

(defun verify-game-end (b)
  "Verify whether the game is done: -1 if lost, 1 if won, else 0"
  (cond ((is-lost b) -1)
	((is-won b) 1)
	(t 0)))

(defun handle-keypress (b scancode)
  "Handles the keypresses for the manual game of snake"
  (cond
    ((sdl2:scancode= scancode :scancode-q)
     (sdl2:push-event :quit))
    ((or (sdl2:scancode= scancode :scancode-up)
	 (sdl2:scancode= scancode :scancode-w))
     (change-dir b *dir-up*))
    ((or (sdl2:scancode= scancode :scancode-down)
	 (sdl2:scancode= scancode :scancode-s))
     (change-dir b *dir-down*))
    ((or (sdl2:scancode= scancode :scancode-left)
	 (sdl2:scancode= scancode :scancode-a))
     (change-dir b *dir-left*))
    ((or (sdl2:scancode= scancode :scancode-right)
	 (sdl2:scancode= scancode :scancode-d))
     (change-dir b *dir-right*))))

(defun render-game (window-size b renderer)
  "Use sdl2 to draw the game on the renderer"
  (draw-game window-size b renderer)
  (sdl2:render-present renderer))

(defun game-loop ()
  "The game loop for interactive games"
  (let ((b *board*)
	(delay-time *delay-time*))
    (do () ((update-game b) nil)
      (sdl2:delay delay-time))
    (sdl2:push-event :quit)))

(defmacro with-game-loop-running (thread-name &body body)
  "Starts the game loop, runs it until `body` returns, then destroys its thread"
  (let ((thread-sym (gensym)))
    `(let ((,thread-sym (bt:make-thread #'game-loop :name ,thread-name)))
       ,@body
       (when (bt:thread-alive-p ,thread-sym)
	 (bt:destroy-thread ,thread-sym)))))

(defun start-game (board-size &key (delay-time 250))
  "Starts an interactive game"
  (let ((b (make-instance 'board :size board-size))
	(window-size (* board-size *cell-size*)))
    (init-game b)
    (setf *delay-time* delay-time)
    (setf *board* b)
    (with-game-loop-running "snake-game-loop"
      (with-initialised-sdl (win renderer) "snek" window-size
	(sdl2:with-event-loop (:method :poll)
	  (:keydown
	   (:keysym keysym)
	   (let ((scancode (sdl2:scancode-value keysym)))
	     (when (handle-keypress b scancode)
	       (update-game b)
	       (render-game window-size b renderer))))
	  (:idle
	   ()
	   (sdl2:delay 10)
	   (render-game window-size b renderer))
	  (:quit () t))))))

(defmethod watch-ai ((sa snake-agent) &key (count 1))
  "Shows the performance of the AI contained in `sa`"
  (with-slots (board each-step) sa
    (let* ((window-size (* (slot-value *board* 'size) *cell-size*))
	   (orig-each-step each-step)
	   (sdl-thread
	    (bt:make-thread
	     (lambda ()
	       (with-initialised-sdl (win renderer) "smart snek" window-size
				     (sdl2:with-event-loop (:method :poll)
				       (:keydown
					(:keysym keysym)
					(let ((scancode (sdl2:scancode-value keysym)))
					  (when (sdl2:scancode= scancode :scancode-space)
					    (if (= *delay-time* 0)
						(setf *delay-time* 200)
						(setf *delay-time* 0)))))
				       (:idle
					()
					(sdl2:delay 10)
					(render-game window-size board renderer))
				       (:quit () t))))
	     :name "snake graphics thread")))

      (setf *delay-time* 200)
      (setf each-step
	    (lambda ()
	      (when orig-each-step
		(funcall orig-each-step))
	      (sdl2:delay *delay-time*)))
      (run-ai sa :count count)
      (setf each-step orig-each-step)
      (sdl2:push-event :quit)
      (bt:destroy-thread sdl-thread))))
