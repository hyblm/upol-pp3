;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; point helpers

(defmethod set-x-y ((point point) x y)
  (setf (slot-value point 'x) x
        (slot-value point 'y) y)
  point)

(defun point-with (x y)
  (set-x-y (make-instance 'point) x y))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída triangle
;;;

;; lecture-1 exercise-2 : def class triangle
(defclass triangle ()
  ((vertex-a :initform (make-instance 'point))
   (vertex-b :initform (make-instance 'point))
   (vertex-c :initform (make-instance 'point))))

;; lecture-1 exercise-3 : def method vertices
(defmethod vertices ((triangle triangle))
  (list (slot-value triangle 'vertex-a)
        (slot-value triangle 'vertex-b)
        (slot-value triangle 'vertex-c)))

;; lecture-1 exercise-3 : def method perimeter
(defmethod perimeter ((triangle triangle))
  (apply #'+ (side-lengths triangle)))

;; lecture-1 exercise-3 : def method right-triangle-p
;;  - pythagoras version
(defmethod right-triangle-p ((triangle triangle) &optional (phi 0.01))
  (destructuring-bind (a b c) (side-lengths triangle)
    (some (lambda (x) (< x phi))
          (list (abs (- (* a a) (+ (* b b) (* c c))))
                (abs (- (* b b) (+ (* c c) (* a a))))
                (abs (- (* c c) (+ (* a a) (* b b))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; triangle helpers

(defmethod set-vertices ((triangle triangle) a b c)
  (setf (slot-value triangle 'vertex-a) a
        (slot-value triangle 'vertex-b) b
        (slot-value triangle 'vertex-c) c) triangle)

(defmethod distance ((p1 point) (p2 point))
  (let ((x (- (slot-value p1 'x) (slot-value p2 'x)))
        (y (- (slot-value p1 'y) (slot-value p2 'y))))
    (sqrt (+ (* x x) (* y y)))))

(defmethod side-lengths ((triangle triangle))
  (let ((a (slot-value triangle 'vertex-a))
        (b (slot-value triangle 'vertex-b))
        (c (slot-value triangle 'vertex-c)))
    (list (distance a b) (distance b c) (distance c a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída ellipse
;;;

(defclass ellipse ()
  ((focal-point-1 :initform (make-instance 'point))
   (focal-point-2 :initform (make-instance 'point))
   (major-semiaxis :initform 1)))

(defmethod major-semiaxis ((ellipse ellipse))
  (slot-value ellipse 'major-semiaxis))

(defmethod minor-semiaxis ((ellipse ellipse))
  (let ((d (/ (distance (slot-value ellipse 'focal-point-1)
                        (slot-value ellipse 'focal-point-2))
              2))
        (ma (slot-value ellipse 'major-semiaxis)))
    (sqrt (- (* ma ma) (* d d)))))

(defmethod set-foci ((ellipse ellipse) (a point) (b point))
  (setf (slot-value ellipse focal-point-1) (a point)
        (slot-value ellipse focal-point-2) (b point)
        ellipse))

(defmethod set-ma ((ellipse ellipse) len)
  (setf (slot-value ellipse 'major-semiaxis) len)
  ellipse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída circle
;;;

(defmethod to-ellipse ((circle circle))
  (let ((c (slot-value circle 'center))
        (r (slot-value circle 'radius)))
    (set-ma (set-foci (make-instance 'ellipse) c c) r)))
