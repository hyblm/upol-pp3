;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Zdrojový soubor k předmětu Paradigmata programování 3
;;;;
;;;; Přednáška 4, Dědičnost
;;;;

#|
Před načtením souboru načtěte knihovnu micro-graphics
Pokud při načítání (kompilaci) dojde k chybě
"Reader cannot find package MG",
znamená to, že knihovna micro-graphics není načtená.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída shape
;;;

#|
Obecná třída všech grafických objektů. Definuje a částečně implementuje
to, co mají společné: vlastnosti související s kreslením (color, thickness,
filledp), geometrické transformace, kreslení.
|#

(defclass shape ()
  ((color :initform :black)
   (thickness :initform 1)
   (filledp :initform nil)))


;;;
;;; Vlastnosti související s kreslením
;;;

(defmethod color ((shape shape)) 
  (slot-value shape 'color))

(defmethod set-color ((shape shape) value) 
  (setf (slot-value shape 'color) value)
  shape)

(defmethod thickness ((shape shape)) 
  (slot-value shape 'thickness)) 

(defmethod set-thickness ((shape shape) value) 
  (setf (slot-value shape 'thickness) value)
  shape) 

(defmethod filledp ((shape shape))
  (slot-value shape 'filledp))

(defmethod set-filledp ((shape shape) value)
  (setf (slot-value shape 'filledp) value)
  shape)


;;;
;;; Kreslení
;;;

;;Pracujeme právě s těmi vlastnostmi, které jsou ve třídě definovány.
(defmethod set-mg-params ((shape shape) mgw) 
  (mg:set-param mgw :foreground (color shape))
  (mg:set-param mgw :filledp (filledp shape))
  (mg:set-param mgw :thickness (thickness shape))
  shape)

(defmethod do-draw ((shape shape) mgw) 
  shape)

;; Základní chování pro každý grafický objekt
(defmethod draw ((shape shape) mgw)
  (set-mg-params shape mgw)
  (do-draw shape mgw))


;;;
;;; Geometrické transformace
;;;

#|
Ve třídě shape není definována žádná geometrie objektů, tak je správné,
když transformace nedělají nic.
|#

(defmethod move ((shape shape) dx dy)
  shape)

(defmethod rotate ((shape shape) angle center)
  shape)

(defmethod scale ((shape shape) coeff center)
  shape)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída point
;;;

(defclass point (shape) 
  ((x :initform 0) 
   (y :initform 0)))


;;;
;;; Geometrie
;;;

(defmethod x ((point point))
  (slot-value point 'x))

(defmethod y ((point point))
  (slot-value point 'y))

(defmethod set-x ((point point) value)
  (unless (typep value 'number)
    (error "x coordinate of a point should be a number"))
  (setf (slot-value point 'x) value)
  point)

(defmethod set-y ((point point) value)
  (unless (typep value 'number)
    (error "y coordinate of a point should be a number"))
  (setf (slot-value point 'y) value)
  point)

(defmethod r ((point point)) 
  (let ((x (slot-value point 'x)) 
        (y (slot-value point 'y))) 
    (sqrt (+ (* x x) (* y y)))))

(defmethod phi ((point point)) 
  (phase (complex (slot-value point 'x)
                  (slot-value point 'y))))

(defmethod set-r-phi ((point point) r phi) 
  (let ((complex (* (cis phi) r)))
    (setf (slot-value point 'x) (realpart complex)
          (slot-value point 'y) (imagpart complex)))
  point)

(defmethod set-r ((point point) value) 
  (set-r-phi point value (phi point)))

(defmethod set-phi ((point point) value) 
  (set-r-phi point (r point) value))


;;;
;;; Kreslení
;;;

;; Nastavení parametrů je netypické - měníme nastavení parametru :filledp
;; ze zděděné metody, protože bod kreslíme jako kolečko
(defmethod set-mg-params ((pt point) mgw)
  (call-next-method)
  (mg:set-param mgw :filledp t)
  pt)

(defmethod do-draw ((pt point) mgw) 
  (mg:draw-circle mgw 
                  (x pt) 
                  (y pt) 
                  (thickness pt))
  pt)


;;;
;;; Geometrické transformace
;;;

(defmethod move ((pt point) dx dy)
  (set-x pt (+ (x pt) dx))
  (set-y pt (+ (y pt) dy))
  pt)

(defmethod rotate ((pt point) angle center)
  (let ((cx (x center))
        (cy (y center)))
    (move pt (- cx) (- cy))
    (set-phi pt (+ (phi pt) angle))
    (move pt cx cy)
    pt))

(defmethod scale ((pt point) coeff center)
  (let ((cx (x center))
        (cy (y center)))
    (move pt (- cx) (- cy))
    (set-r pt (* (r pt) coeff))
    (move pt cx cy)
    pt))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída circle
;;;

(defclass circle (shape) 
  ((center :initform (make-instance 'point)) 
   (radius :initform 1)))


;;;
;;; Geometrie
;;;

(defmethod radius ((c circle))
  (slot-value c 'radius))

(defmethod set-radius ((c circle) value)
  (when (< value 0)
    (error "Circle radius should be a non-negative number"))
  (setf (slot-value c 'radius) value)
  c)

(defmethod center ((c circle))
  (slot-value c 'center))


;;;
;;; Kreslení
;;;

(defmethod do-draw ((c circle) mg-window)
  (mg:draw-circle mg-window
                  (x (center c))
                  (y (center c))
                  (radius c))
  c)


;;;
;;; Geometrické transformace
;;;

(defmethod move ((c circle) dx dy)
  (move (center c) dx dy)
  c)

(defmethod rotate ((c circle) angle center)
  (rotate (center c) angle center)
  c)

(defmethod scale ((c circle) coeff center)
  (scale (center c) coeff center)
  (set-radius c (* (radius c) coeff))
  c)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída compound-shape
;;;

#|
Třída compound-shape slouží jako předek složených grafických objektů, tedy 
těch, co mají vlastnost items.

Nepředpokládáme vytváření přímých instancí.
|#

(defclass compound-shape (shape)
  (items))


;;;
;;; Práce s items
;;;

(defmethod items ((shape compound-shape)) 
  (copy-list (slot-value shape 'items)))

;; Pomocná zpráva, posílá danou zprávu s danými argumenty všem prvkům
(defmethod send-to-items ((shape compound-shape) 
			  message
			  &rest arguments)
  (dolist (item (items shape))
    (apply message item arguments))
  shape)

(defmethod check-item ((shape compound-shape) item)
  (error "Method check-item of compound-shape must be rewritten."))

(defmethod do-check-items ((shape compound-shape) item-list)
  (dolist (item item-list)
    (check-item shape item))
  shape)

(defmethod check-items ((shape compound-shape) item-list)
  (error "Method check-items of compound-shape must be rewritten.")
  shape)

(defmethod set-items ((shape compound-shape) value)
  (check-items shape value)
  (setf (slot-value shape 'items) (copy-list value))
  shape)


;;;
;;; Geometrické transformace
;;;

(defmethod move ((shape compound-shape) dx dy)
  (send-to-items shape 'move dx dy)
  shape)

(defmethod rotate ((shape compound-shape) angle center)
  (send-to-items shape 'rotate angle center)
  shape)

(defmethod scale ((shape compound-shape) coeff center)
  (send-to-items shape 'scale coeff center)
  shape)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída picture
;;;

(defclass picture (compound-shape)
  ((items :initform '())))


;;;
;;; Práce s items
;;;

(defmethod check-item ((p picture) item)
  (unless (typep item 'shape)
    (error "Items of picture must be shapes.")))

(defmethod check-items ((p picture) items)
  (do-check-items p items))


;;;
;;; Kreslení
;;;

(defmethod draw ((pic picture) mg-window)
  (dolist (item (reverse (items pic)))
    (draw item mg-window))
  pic)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída polygon
;;;

#|
Proti třídě shape obsahuje polygon novou grafickou vlastnost: closedp.
Musíme ji tedy definovat (nový slot, přístupové metody, doplnění do 
set-mg-params).
|#

(defclass polygon (compound-shape)
  ((items :initform '())
   (closedp :initform t)))

(defmethod closedp ((p polygon))
  (slot-value p 'closedp))

(defmethod set-closedp ((p polygon) value)
  (setf (slot-value p 'closedp) value)
  p)


;;;
;;; Práce s items
;;;

(defmethod check-item ((p polygon) item)
  (unless (typep item 'point)
    (error "Items of polygon must be points.")))

(defmethod check-items ((p polygon) items)
  (do-check-items p items))


;;;
;;; Kreslení
;;;

(defmethod set-mg-params ((p polygon) mgw) 
  (call-next-method)
  (mg:set-param mgw :closedp (closedp p))
  p)

(defmethod polygon-coordinates ((p polygon))
  (let (coordinates)
    (dolist (point (reverse (items p)))
      (setf coordinates (cons (y point) coordinates)
            coordinates (cons (x point) coordinates)))
    coordinates))

(defmethod do-draw ((poly polygon) mg-window) 
  (mg:draw-polygon mg-window 
                   (polygon-coordinates poly))
  poly)

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída window
;;;

#|
Třída zůstává beze změny.
|#

(defclass window ()
  ((mg-window :initform (mg:display-window))
   (shape :initform nil)
   (background :initform :white)))

(defmethod shape ((w window))
  (slot-value w 'shape))

(defmethod set-shape ((w window) shape)
  (setf (slot-value w 'shape) shape)
  w)

(defmethod background ((w window))
  (slot-value w 'background))

(defmethod set-background ((w window) color)
  (setf (slot-value w 'background) color)
  w)

(defmethod redraw ((window window))
  (let ((mgw (slot-value window 'mg-window)))
    (mg:set-param mgw :background (background window))
    (mg:clear mgw)
    (when (shape window)
      (draw (shape window) mgw)))
  window)



