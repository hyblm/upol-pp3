;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Zdrojový soubor k předmětu Paradigmata programování 3
;;;;
;;;; Přednáška 5, Návrh stromu dědičnosti
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

Nepředpokládáme vytváření přímých instancí. Práce s vlastností items je
připravena, ale je zařízeno, aby items nešlo nastavit - prekondice metody
set-items není nikdy splněna.
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
  (error "Invalid compound-shape item"))

(defmethod do-check-items ((shape compound-shape) item-list)
  (dolist (item item-list)
    (check-item shape item))
  shape)

(defmethod check-items ((shape compound-shape) item-list)
  (error "Invalid items."))

(defmethod set-items ((shape compound-shape) value)
  (check-items shape value)
  (do-set-items shape value)
  shape)

(defmethod do-set-items ((shape compound-shape) value)
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
;;; Třída abstract-picture
;;;

#|
U některých obrázků nechceme, aby uživatel mohl nastavovat items, protože
by je uvedl do nekonzistentního stavu (třeba u bulls-eye). Ty budou potomky
přímo této třídy.

Instance takových tříd budou s items pracovat přes interní zprávu
do-set-items.
|#

(defclass abstract-picture (compound-shape)
  ((items :initform '())))


;;;
;;; Práce s items
;;;

(defmethod check-item ((p abstract-picture) item)
  (unless (typep item 'shape)
    (error "Invalid picture element type."))
  p)


;;;
;;; Kreslení
;;;

(defmethod draw ((pic abstract-picture) mg-window)
  (dolist (item (reverse (items pic)))
    (draw item mg-window))
  pic)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída picture
;;;

#|
Tato třída povoluje u obrázků libovolné nastavování items.
|#

(defclass picture (abstract-picture)
  ())

(defmethod check-items ((p picture) item-list)
  (do-check-items p item-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída abstract-polygon
;;; 

#|
U některých polygonů nechceme, aby uživatel mohl nastavovat items, protože
by je uvedl do nekonzistentního stavu (třída čtyřúhelník). Ty budou potomky
přímo této třídy.

Instance takových tříd budou s items pracovat přímo jako se slotem.
- to je volnější princip zapouzdření.
|#

#|
Proti třídě shape obsahuje polygon novou grafickou vlastnost: closedp.
Musíme ji tedy definovat (nový slot, přístupové metody, doplnění do 
set-mg-params).
|#

(defclass abstract-polygon (compound-shape)
  ((closedp :initform t)))

(defmethod check-item ((p abstract-polygon) item)
  (unless (typep item 'point)
    (error "Invalid polygon element type."))
  p)

(defmethod closedp ((p abstract-polygon))
  (slot-value p 'closedp))

(defmethod set-closedp ((p abstract-polygon) value)
  (setf (slot-value p 'closedp) value)
  p)


;;;
;;; Kreslení
;;;

(defmethod set-mg-params ((p abstract-polygon) mgw) 
  (call-next-method)
  (mg:set-param mgw :closedp (closedp p))
  p)

(defmethod polygon-coordinates ((p abstract-polygon))
  (let (coordinates)
    (dolist (point (reverse (items p)))
      (setf coordinates (cons (y point) coordinates)
            coordinates (cons (x point) coordinates)))
    coordinates))

(defmethod do-draw ((poly abstract-polygon) mg-window) 
  (mg:draw-polygon mg-window 
                   (polygon-coordinates poly))
  poly)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída polygon
;;;

#|
Tato třída povoluje u polygonů libovolné nastavování items.
|#
 
(defclass polygon (abstract-polygon)
  ())

(defmethod check-items ((p polygon) item-list)
  (do-check-items p item-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída abstract-window
;;;

(defclass abstract-window ()
  ((mg-window :initform (mg:display-window))
   (shape :initform nil)
   (background :initform :white)))

(defmethod shape ((w abstract-window))
  (slot-value w 'shape))

(defmethod check-shape ((w abstract-window) shape)
  (error "Cannot set shape of abstract-window"))

(defmethod set-shape ((w abstract-window) shape)
  (check-shape w shape)
  (do-set-shape w shape))

(defmethod do-set-shape ((w abstract-window) shape)
  (setf (slot-value w 'shape) shape)
  w)

(defmethod background ((w abstract-window))
  (slot-value w 'background))

(defmethod set-background ((w abstract-window) color)
  (setf (slot-value w 'background) color)
  w)

(defmethod redraw ((window abstract-window))
  (let ((mgw (slot-value window 'mg-window)))
    (mg:set-param mgw :background (background window))
    (mg:clear mgw)
    (when (shape window)
      (draw (shape window) mgw)))
  window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída window
;;;

(defclass window (abstract-window)
  ())

(defmethod check-shape ((w window) shape)
  t)



