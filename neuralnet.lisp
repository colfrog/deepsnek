(defclass neuron ())
(defclass layer ())
(defclass network ())

(defun sigmoid (x)
  (/ 1 (+ 1 (exp (- x)))))

(defun sigmoid-derivative (x)
  (* (sigmoid x) (- 1 (sigmoid x))))
