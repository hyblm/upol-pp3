;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Zdrojový soubor k předmětu Paradigmata programování 3
;;;;
;;;; Přednáška 8, Princip vlastnění
;;;;

#|
Před načtením souboru načtěte knihovnu micro-graphics
Pokud při načítání (kompilaci) dojde k chybě
"Reader cannot find package MG",
znamená to, že knihovna micro-graphics není načtená.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída omg-object
;;;

(defclass omg-object () 
  ((delegate :initform nil)
   (change-level :initform 0)
   (events :initform '(ev-changing ev-change ev-mouse-down))))

(defmethod delegate ((obj omg-object))
  (slot-value obj 'delegate))

(defmethod set-delegate ((obj omg-object) delegate)
  (setf (slot-value obj 'delegate) delegate))

(defmethod change-level ((obj omg-object))
  (slot-value obj 'change-level))

(defmethod inc-change-level ((obj omg-object))
  (incf (slot-value obj 'change-level))
  obj)

(defmethod dec-change-level ((obj omg-object))
  (decf (slot-value obj 'change-level))
  obj)


;; Vlastnost events nastavuje delegát. Tím určí, jaké události
;; mu má objekt posílat.
(defmethod events ((obj omg-object))
  (slot-value obj 'events))

(defmethod add-event ((obj omg-object) event)
  (unless (find event (events obj))
    (setf (slot-value obj 'events)
          (cons event (events obj))))
  obj)

(defmethod remove-event ((obj omg-object) event)
  (setf (slot-value obj 'events)
        (remove event (events obj))))


;; posílání událostí: send-event

(defmethod send-event ((object omg-object) event &rest event-args)
  (let ((delegate (delegate object)))
    (when (and delegate (find event (events object)))
      (apply event delegate object event-args))
    object))

(defmethod changing ((object omg-object) origin msg &rest args)
  (when (zerop (change-level object))
    (apply 'send-event object 'ev-changing origin msg args))
  (inc-change-level object))

(defmethod change ((object omg-object) origin msg &rest args)
  (dec-change-level object)
  (when (zerop (change-level object))
    (apply 'send-event object 'ev-change origin msg args)))

(defmethod send-with-change ((obj omg-object) msg orig-msg 
                             &rest args)
  (apply 'changing obj obj orig-msg args)
  (unwind-protect
       (apply msg obj args)
    (apply 'change obj obj orig-msg args))
  obj)


;; základní události

(defmethod ev-changing ((obj omg-object) sender origin message 
                        &rest msg-args)
  (apply 'changing obj origin message msg-args))

(defmethod ev-change ((obj omg-object) sender origin message 
                      &rest msg-args)
  (apply 'change obj origin message msg-args))

(defmethod ev-mouse-down 
    ((obj omg-object) sender clicked button position)
  (send-event obj 'ev-mouse-down clicked button position))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída shape
;;;

#|
Obecná třída všech grafických objektů. Definuje a částečně implementuje
to, co mají společné: vlastnosti související s kreslením (color, thickness,
filledp), geometrické transformace, kreslení.

Dále hlášení změn oknu a práci s myší.
|#

(defclass shape (omg-object)
  ((color :initform :black)
   (thickness :initform 1)
   (filledp :initform nil)))


;;;
;;; Vlastnosti související s kreslením
;;;

(defmethod color ((shape shape)) 
  (slot-value shape 'color))

(defmethod do-set-color ((shape shape) value)
  (setf (slot-value shape 'color) value))

(defmethod set-color ((shape shape) value) 
  (send-with-change shape 'do-set-color 'set-color value))

(defmethod thickness ((shape shape)) 
  (slot-value shape 'thickness)) 

(defmethod do-set-thickness ((shape shape) value)
  (setf (slot-value shape 'thickness) value))

(defmethod set-thickness ((shape shape) value) 
  (send-with-change shape 'do-set-thickness 'set-thickness value))

(defmethod filledp ((shape shape))
  (slot-value shape 'filledp))

(defmethod do-set-filledp ((shape shape) value)
  (setf (slot-value shape 'filledp) value))

(defmethod set-filledp ((shape shape) value) 
  (send-with-change shape 'do-set-filledp 'set-filledp value)) 


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
  (send-with-change shape 'do-move 'move dx dy))

(defmethod do-move ((shape shape) dx dy)
  shape)

(defmethod rotate ((shape shape) angle center)
  (send-with-change shape 'do-rotate 'rotate angle center))

(defmethod do-rotate ((shape shape) angle center)
  shape)

(defmethod scale ((shape shape) coeff center)
  (send-with-change shape 'do-scale 'scale coeff center))

(defmethod do-scale ((shape shape) coeff center)
  shape)


;;;
;;; Práce s myší
;;;

;; Každý objekt je defaultně solidp. Přepsáno ve třídě abstract-picture
(defmethod solidp ((shape shape))
  t)

(defmethod solid-shapes ((shape shape))
  (if (solidp shape)
      (list shape)
      (solid-subshapes shape)))


;; Přepsat ve třídách, kde solidp může být nil
(defmethod solid-subshapes ((shape shape))
  (error "Method has to be rewritten."))

(defmethod contains-point-p ((shape shape) point)
  (error "Method has to be rewritten."))

(defmethod mouse-down ((shape shape) button position)
  (send-event shape 'ev-mouse-down shape button position))


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
  (send-with-change point 'do-set-x 'set-x value))

(defmethod do-set-x ((point point) value)
  (setf (slot-value point 'x) value))

(defmethod set-y ((point point) value)
  (unless (typep value 'number)
    (error "y coordinate of a point should be a number"))
  (send-with-change point 'do-set-y 'set-y value))

(defmethod do-set-y ((point point) value)
  (setf (slot-value point 'y) value))

(defmethod r ((point point)) 
  (let ((x (slot-value point 'x)) 
        (y (slot-value point 'y))) 
    (sqrt (+ (* x x) (* y y)))))

(defmethod phi ((point point)) 
  (let ((x (slot-value point 'x)) 
        (y (slot-value point 'y))) 
    (cond ((> x 0) (atan (/ y x))) 
          ((< x 0) (+ pi (atan (/ y x)))) 
          (t (* (signum y) (/ pi 2))))))


(defmethod do-set-r-phi ((point point) r phi) 
  (setf (slot-value point 'x) (* r (cos phi)) 
        (slot-value point 'y) (* r (sin phi))) 
  point)

(defmethod set-r-phi ((point point) r phi) 
  (send-with-change point 'do-set-r-phi 'set-r-phi r phi))

(defmethod do-set-r ((point point) value) 
  (set-r-phi point value (phi point)))

(defmethod set-r ((point point) r) 
  (send-with-change point 'do-set-r 'set-r r))

(defmethod do-set-phi ((point point) value) 
  (set-r-phi point (r point) value))

(defmethod set-phi ((point point) phi) 
  (send-with-change point 'do-set-phi 'set-phi phi))

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

(defmethod do-move ((pt point) dx dy)
  (set-x pt (+ (x pt) dx))
  (set-y pt (+ (y pt) dy))
  pt)

(defmethod do-rotate ((pt point) angle center)
  (let ((cx (x center))
        (cy (y center)))
    (move pt (- cx) (- cy))
    (set-phi pt (+ (phi pt) angle))
    (move pt cx cy)
    pt))

(defmethod do-scale ((pt point) coeff center)
  (let ((cx (x center))
        (cy (y center)))
    (move pt (- cx) (- cy))
    (set-r pt (* (r pt) coeff))
    (move pt cx cy)
    pt))


;;;
;;; Práce s myší
;;;

;; Pomocné funkce (vzdálenost bodů)

(defun sqr (x)
  (expt x 2))

(defun point-dist (pt1 pt2)
  (sqrt (+ (sqr (- (x pt1) (x pt2)))
           (sqr (- (y pt1) (y pt2))))))

(defmethod contains-point-p ((shape point) point)
  (<= (point-dist shape point) 
      (thickness shape)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída circle
;;;

(defclass circle (shape) 
  ((center :initform (make-instance 'point)) 
   (radius :initform 1)))

(defmethod initialize-instance ((c circle) &key)
  (call-next-method)
  (set-delegate (center c) c)
  c)


;;;
;;; Geometrie
;;;

(defmethod radius ((c circle))
  (slot-value c 'radius))

(defmethod set-radius ((c circle) value)
  (when (< value 0)
    (error "Circle radius should be a non-negative number"))
  (send-with-change c 'do-set-radius 'set-radius value))

(defmethod do-set-radius ((c circle) value)
  (setf (slot-value c 'radius) value))

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

(defmethod do-move ((c circle) dx dy)
  (move (center c) dx dy)
  c)

(defmethod do-rotate ((c circle) angle center)
  (rotate (center c) angle center)
  c)

(defmethod do-scale ((c circle) coeff center)
  (scale (center c) coeff center)
  (set-radius c (* (radius c) coeff))
  c)


;;;
;;; Práce s myší
;;;

(defmethod contains-point-p ((circle circle) point)
  (let ((dist (point-dist (center circle) point))
        (half-thickness (/ (thickness circle) 2)))
    (if (filledp circle)
        (<= dist (radius circle))
        (<= (- (radius circle) half-thickness)
            dist
            (+ (radius circle) half-thickness)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída compound-shape
;;;

#|
Třída compound-shape slouží jako předek složených grafických objektů, tedy 
těch, co mají vlastnost items.

Nepředpokládáme vytváření přímých instancí. Práce s vlastností items je
připravena, ale je zařízeno, aby items nešlo nastavit - prekondice metody
set-shape není nikdy splněna.
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
  (send-with-change shape 'do-set-items 'set-items value))

(defmethod do-set-items ((shape compound-shape) value)
  (setf (slot-value shape 'items) (copy-list value))
  (send-to-items shape 'set-delegate shape))


;;;
;;; Geometrické transformace
;;;

(defmethod do-move ((shape compound-shape) dx dy)
  (send-to-items shape 'move dx dy)
  shape)

(defmethod do-rotate ((shape compound-shape) angle center)
  (send-to-items shape 'rotate angle center)
  shape)

(defmethod do-scale ((shape compound-shape) coeff center)
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


;;;
;;; Práce s myší
;;;

(defmethod solidp ((pic abstract-picture))
  nil)

(defmethod solid-subshapes ((shape abstract-picture))
  (mapcan 'solid-shapes (items shape)))

(defmethod contains-point-p ((pic abstract-picture) point)
  (find-if (lambda (item)
             (contains-point-p item point))
           (items pic)))


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

Instance takových tříd budou s items pracovat přes interní zprávu
do-set-items
|#

#|
Proti třídě shape obsahuje polygon novou grafickou vlastnost: closedp.
Musíme ji tedy definovat (nový slot, přístupové metody, doplnění do 
set-mg-params).
|#
(defclass abstract-polygon (compound-shape)
  ((items :initform '())
   (closedp :initform t)))

(defmethod check-item ((p abstract-polygon) item)
  (unless (typep item 'point)
    (error "Invalid polygon element type."))
  p)

(defmethod closedp ((p abstract-polygon))
  (slot-value p 'closedp))

(defmethod set-closedp ((p abstract-polygon) value) 
  (send-with-change p 'do-set-closedp 'set-closedp value))

(defmethod do-set-closedp ((p abstract-polygon) value)
  (setf (slot-value p 'closedp) value))


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


;;;
;;; práce s myší
;;;

;;
;; contains-point-p pro polygon využívá funkci
;; mg:point-in-polygon-p knihovny micro-graphics.
;;

(defmethod contains-point-p ((poly abstract-polygon) point)
  (mg:point-in-polygon-p (x point) (y point) 
                         (closedp poly)
                         (filledp poly) 
                         (thickness poly)
                         (polygon-coordinates poly)))


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

(defclass abstract-window (omg-object)
  ((mg-window :initform (mg:display-window))
   (shape :initform nil)
   (background :initform :white)))

(defmethod initialize-instance ((w abstract-window) &key)
  (call-next-method)
  (install-callbacks w)
  w)

(defmethod install-callbacks ((w abstract-window))
  (install-display-callback w)
  (install-mouse-down-callback w))

(defmethod install-display-callback ((w abstract-window))
  (mg:set-callback (slot-value w 'mg-window)
                   :display (lambda (mgw)
                              (declare (ignore mgw))
                              (redraw w)))
  w)

(defmethod install-mouse-down-callback ((w abstract-window))
  (mg:set-callback 
   (slot-value w 'mg-window) 
   :mouse-down (lambda (mgw button x y)
                 (declare (ignore mgw))
                 (window-mouse-down
                  w
                  button 
                  (move (make-instance 'point) x y))))
  w)

(defmethod shape ((w abstract-window))
  (slot-value w 'shape))

(defmethod check-shape ((w abstract-window) shape)
  (error "Cannot set shape of abstract-window"))

(defmethod set-shape ((w abstract-window) shape)
  (check-shape w shape)
  (send-with-change w 'do-set-shape 'set-shape shape))

(defmethod do-set-shape ((w abstract-window) s)
  (setf (slot-value w 'shape) s)
  (when s (set-delegate s w))
  w)

(defmethod background ((w abstract-window))
  (slot-value w 'background))

(defmethod set-background ((w abstract-window) color)
  (send-with-change w 'do-set-background 'set-background color))

(defmethod do-set-background ((w abstract-window) color)
  (setf (slot-value w 'background) color))

(defmethod redraw ((window abstract-window))
  (let ((mgw (slot-value window 'mg-window)))
    (mg:set-param mgw :background (background window))
    (mg:clear mgw)
    (when (shape window)
      (draw (shape window) mgw)))
  window)


;;;
;;; Změny
;;;

(defmethod invalidate ((w abstract-window))
  (mg:invalidate (slot-value w 'mg-window))
  w)

(defmethod change ((w abstract-window) origin message 
                   &rest msg-args)
  (call-next-method)
  (invalidate w))


;;;
;;; Klikání
;;;

(defmethod find-clicked-shape ((w abstract-window) position)
  (when (shape w)
    (find-if (lambda (shape) (contains-point-p shape position))
             (solid-shapes (shape w)))))

(defmethod mouse-down-inside-shape 
    ((w abstract-window) shape button position)
  (mouse-down shape button position)
  w)

(defmethod mouse-down-no-shape 
    ((w abstract-window) button position)
  w)

(defmethod window-mouse-down ((w abstract-window) button position)
  (let ((shape (find-clicked-shape w position)))
    (if shape
        (mouse-down-inside-shape w shape button position)
        (mouse-down-no-shape w button position))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Třída window
;;;

(defclass window (abstract-window)
  ())

(defmethod check-shape ((w window) shape)
  t)



