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

(defun make-custom-rect (x y params)
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
  (let* ((apple (get-apple b))
	 (x (* *cell-size* (car apple)))
	 (y (* *cell-size* (cdr apple)))
	 (rect (make-custom-rect x y (aref *snake-parts* 0))))
    (sdl2:set-render-draw-color renderer 255 0 0 0)
    (sdl2:render-fill-rect renderer rect)))

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
  (draw-game window-size b renderer)
  (sdl2:render-present renderer))

(defun game-loop ()
  (let ((b *board*)
	(delay-time *delay-time*))
    (do () ((update-game b) nil)
      (sdl2:delay delay-time))
    (sdl2:push-event :quit)))

(defmacro with-game-loop-running (thread-name &body body)
  (let ((thread-sym (gensym)))
    `(let ((,thread-sym (bt:make-thread #'game-loop :name ,thread-name)))
       ,@body
       (when (bt:thread-alive-p ,thread-sym)
	 (bt:destroy-thread ,thread-sym)))))

(defun start-game (board-size &key (delay-time 250))
  (let ((b (make-instance 'board :size board-size))
	(window-size (* board-size *cell-size*)))
    (init-board b)
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
