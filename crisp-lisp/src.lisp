;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída triangle
;;;

(defclass triangle ()
  ((vertex-a :initform (make-instance 'point))
   (vertex-b :initform (make-instance 'point))
   (vertex-c :initform (make-instance 'point))))

;; Vrátí seznam vrcholů
(defmethod vertices ((triangle triangle))
  (list (slot-value triangle 'vertex-a)
        (slot-value triangle 'vertex-b)
        (slot-value triangle 'vertex-c)))

(defun hyp (x y)
  (sqrt (+ (* x x) (* y y))))

(defmethod distance ((p1 point) (p2 point))
  (let ((x  (- (slot-value p1 'x) (slot-value p2 'x)))
        (y  (- (slot-value p1 'y) (slot-value p2 'y))))
      (sqrt (+ (* x x) (* y y)))))

(defmethod side-lengths ((triangle triangle))
(let ((a (slot-value triangle 'vertex-a))
      (b (slot-value triangle 'vertex-b))
      (c (slot-value triangle 'vertex-c)))
  (list (distance a b) (distance b c) (distance c a)))
)

;; Vrátí délku obvodu
(defmethod perimeter ((triangle triangle))
  (apply #'+ (side-lengths triangle)))

;; Je trojuhelník pravoúhlý
(defmethod right-triangle-p ((triangle triangle) &optional (phi 0.01))
  (destructuring-bind (a b c) (side-lengths triangle)
    (some (labmda (x) (< x phi))
      (- (* a a) (+ (* b b) (* c c)))
      (- (* b b) (+ (* c c) (* a a)))
      (- (* c c) (+ (* a a) (* b b))))))

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

(defmethod set-foci ((ellipse ellipse) a b)
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
