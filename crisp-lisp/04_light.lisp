;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; 04_light.lisp -  - příklad k přednášce 4. Dědičnost
;;;;

#|

DOKUMENTACE
-----------

Třída light je potomkem třídy circle. Její instance představují kulaté světlo,
které lze zapnout a vypnout. 

NOVÉ VLASTNOSTI

on-color:   Barva zapnutého světla.
off-color:  Barva vypnutého světla.
onp:        Logická hodnota "světlo je zapnuté"

NOVÉ ZPRÁVY

turn-on      Zapne světlo (bez parametru)
turn-off     Vypne světlo (bez parametru)
toggle       Zapnuté světlo vypne, vypnuté zapne (bez parametru)

Projděte si testovací kód na konci souboru.
|#

(defclass light (circle)
  ((onp :initform t)
   (on-color :initform :red)
   (off-color :initform :grey)))

(defmethod onp ((l light))
  (slot-value l 'onp))

(defmethod ensure-color ((l light))
  (set-color l 
             (slot-value l (if (onp l)
                               'on-color
                             'off-color))))

(defmethod initialize-instance ((l light) &key)
  (call-next-method)
  (set-filledp l t)
  (ensure-color l))

(defmethod set-onp ((l light) value)
  (setf (slot-value l 'onp) value)
  (ensure-color l))

(defmethod turn-on ((l light))
  (set-onp l t))

(defmethod turn-off ((l light))
  (set-onp l nil))

(defmethod toggle ((l light))
  (set-onp l (not (onp l))))

(defmethod on-color ((l light))
  (slot-value l 'on-color))

(defmethod set-on-color ((l light) value)
  (setf (slot-value l 'on-color) value)
  (ensure-color l))

(defmethod off-color ((l light))
  (slot-value l 'off-color))

(defmethod set-off-color ((l light) value)
  (setf (slot-value l 'off-color) value)
  (ensure-color l))

#|
;; Testy třídy light (každý výraz vyhodnocujte zvlášť tlačítkem F8):

#|
;; Uživatelé Mac OS místo redraw použijí:

(mg:drawing (slot-value w 'mg-window)
  (redraw w))
|#

(setf w (make-instance 'window))

(setf light (move (set-radius (make-instance 'light)
                              50)
                  100 100))

(set-shape w light)
(redraw w)

(toggle light)
(redraw w)

(set-on-color light :green)
(redraw w)

(turn-on light)
(redraw w)
|#