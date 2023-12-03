#|

DOUBLE-CLICK EVENT
----------------------------

|#

(defmethod ev-double-click
    ((obj omg-object) sender clicked button position)
  (send-event obj 'ev-double-click clicked button position))

(defmethod double-click ((shape shape) button position)
  (send-event shape 'ev-double-click shape button position))

(defmethod install-double-click-callback ((w abstract-window))
  (mg:set-callback
   (slot-value w 'mg-window)
   :double-click (lambda (mgw button x y)
                   (declare (ignore mgw))
                   (window-double-click
                    w
                    button
                    (move (make-instance 'point) x y))))
  w)

(defmethod window-double-click ((w abstract-window) button position)
  (let ((shape (find-clicked-shape w position)))
    (if shape
        (double-click-inside-shape w shape button position)
        (double-click-no-shape w button position))))

(defmethod double-click-inside-shape
    ((w abstract-window) shape button position)
  (double-click shape button position)
  w)

(defmethod double-click-no-shape
    ((w abstract-window) button position)
  w)

#|

DOKUMENTACE
-----------

TŘÍDA SELECT-WINDOW (WINDOW)
----------------------------

Okno s možností vybrat jednotlivé objekty (vlastnost selection). Objekty v okně
lze vybrat kliknutím myši, vždy je vybraný pouze jeden objekt. Výběr se zruší
kliknutím na pozadí okna. Vybraný objekt je zvýrazněný obdelníkvým obrysem.


NOVÁ VLASTNOST

selection   ::   Pod-objekt okna, který je v danou chvíli výběrem

|#

(defclass select-window (window)
  ((selection :initform nil)))

(defmethod selection ((w select-window))
  (slot-value w 'selection))

(defmethod do-set-selection ((w select-window) selection)
  (setf (slot-value w 'selection) selection))

(defmethod set-selection ((w select-window) selection)
  (send-with-change w 'do-set-selection 'set-selection selection))

(defmethod redraw ((w select-window))
  (call-next-method)
  (let ((mgw (slot-value w 'mg-window))
        (selected (selection w)))
    (when selected
      (draw (border selected :white 5) mgw)
      (draw (border selected :black 4) mgw)
      )))

(defmethod ev-mouse-down
    ((w select-window) sender clicked button position)
  (set-selection w clicked))

(defmethod mouse-down-no-shape
    ((w select-window) button position)
  (set-selection w nil))

(defmethod border ((shape shape) color offset)
  (let ((left (- (left shape) offset))
        (right (+ (right shape) offset))
        (top (- (top shape) offset))
        (bottom (+ (bottom shape) offset)))
    (set-items (set-color (set-closedp (make-instance 'polygon) t) color)
               (mapcar (lambda (x y)
                         (move (make-instance 'point) x y))
                       (list left right right left)
                       (list top top bottom bottom)))))

#| Test   ::   SELECT-WINDOW

(setf w (make-instance 'select-window))

(setf pic (move (set-items (make-instance 'picture)
(list
(move (set-radius (set-filledp (make-instance 'circle) t) 40) 100 100)
(set-radius (set-filledp (make-instance 'circle) t) 60)
) ) 100 100))

(set-shape w pic)

|#

#|

DOKUMENTACE
-----------

TŘÍDA INSPECTED-WINDOW (SELECT-WINDOW)
--------------------------------------

|#

(defclass inspected-window (select-window)
  ())

#|

DOKUMENTACE
-----------

TŘÍDA INSPECTOR (ABSTRACT-PICTURE)
----------------------------------

|#

(defclass inspector (abstract-picture)
  ())

(defconstant *inspector-header-height* 170)
(defconstant *observation-height* 30)

(defmethod initialize-instance ((inspector inspector) &key)
  (call-next-method)
  (do-set-items inspector (list (set-text (set-color
                                           (make-instance 'text-shape)
                                           :white)
                                          "
                  _
               _|  |
             .'_)   |
            /_/ |_|
           (_)  |_|          __--============--__
           \\  \\______    |        INSPECTOR         |
          (________)  |__--============--__|
                                          "))))

(defmethod observe ((inspector inspector) (object omg-object))
  (send-with-change inspector 'do-observe 'observe object))

(defmethod do-observe ((inspector inspector) (object omg-object))
  (do-set-items inspector (cons (first (items inspector))
                                (make-prop-list inspector object)))
  (send-to-items inspector 'set-delegate inspector)
  (send-to-items inspector 'add-event 'ev-double-click))

(defmethod update ((inspector inspector) shape prop)
  (send-with-change inspector 'do-update 'update shape prop))

(defmethod do-update ((inspector inspector) shape prop)
  (update-text (find prop (cddr (items inspector))
                     :test (lambda (a b)
                             (eql a (prop-sym b))))
               (funcall prop shape)))

(defmethod make-prop-list ((inspector inspector) shape)
  (let ((prop-list
          (cons (set-text (set-color (make-instance 'text-shape) :white)
                          (format nil "~a" (type-of shape)))
                (make-prop-buttons inspector shape))))
    (mapcar (lambda (line dy) (move line 10 dy))
            prop-list
            (loop :for i :from *inspector-header-height* :by *observation-height*
                  :repeat (length prop-list)
                  :collect i))))

(defmethod make-prop-buttons ((inspector inspector) shape)
  (mapcar (lambda (prop)
            (make-instance 'prop-button :prop prop :value (funcall prop shape)))
          (props shape)))

#|

DOKUMENTACE
-----------


TŘÍDA PROP-BUTTON (TEXT-SHAPE)
------------------------------

Text shape vypisující název a hodnotu vlastnosti nejakeho objektu.


NOVÁ VLASTNOST

prop-sym   ::   symbol vlastnosti, o které PROP-BUTTON vypisuje hodnotu


|#

(defclass prop-button (text-shape)
  ((prop-sym :initform nil)))

(defmethod initialize-instance ((pb prop-button) &key prop value)
  (call-next-method)
  (set-color pb (if (setter-name prop) :white :grey))
  (setf (slot-value pb 'prop-sym) prop)
  (set-text pb (format nil "~a: ~a" prop value)))

(defmethod prop-sym ((pb prop-button))
  (slot-value pb 'prop-sym))

(defmethod update-text ((pb prop-button) value)
  (set-text pb (format nil "~a: ~a" (prop-sym pb) value)))

#|

DOKUMENTACE
-----------

METODY PROPS PRO GRAFICKÉ OBJEKTY KNIHOVNY OMG
----------------------------------------------

Aby nový grafický objekt odhalil inspektoru nové vlastnosti,
musí přepsat metodu OBJECT-PROPS.

Ta by měla vždy vracet seznam nových vlastností grafického objektu.
Aby se automaticky zobrazovali i zděděné vlastnosti, použijte makro NEW-PROPS
e.g.:
;;    (defmethod props ((object my-object))
;;      (new-props 'new-prop 'new-prop2))

Po expanzi makra:
;;    (defmethod props ((object my-object))
;;      (append (list 'new-prop 'new-prop2) (when (next-method-p)
;;                                            (call-next-method))))

|#

(defmacro new-props (&rest props)
  `(append (list ,@props) (when (next-method-p) (call-next-method))))

(defgeneric props (object)
  (:method ((object omg-object))
    nil)
  (:method ((object shape))
    (new-props 'color 'thickness 'filledp))
  (:method ((object point))
    (new-props 'x 'y 'r 'phi))
  (:method ((object circle))
    (new-props 'center 'radius))
  (:method ((object compound-shape))
    (new-props 'items))
  (:method ((object abstract-polygon))
    (new-props 'closedp))
  (:method ((object abstract-window))
    (new-props 'shape 'background)))

#|

DOKUMENTACE
-----------

TŘÍDA INSPECTOR-WINDOW (ABSTRACT-WINDOW)
----------------------------------------

|#

(defclass inspector-window (abstract-window)
  ((inspected-window :initform nil)))

(defmethod initialize-instance ((w inspector-window) &key)
  (call-next-method)
  (do-set-shape w (make-instance 'inspector))
  (add-event (shape w) 'ev-double-click)
  (set-background w :grey10)
  w)

(defmethod install-callbacks ((w inspector-window))
  (call-next-method)
  (install-double-click-callback w))

(defmethod ev-change ((w inspector-window) sender origin message
                      &rest msg-args)
  (let ((inspected-window (inspected-window w))
        (inspected-object (inspected-object w)))
    (when (eql sender inspected-window)
      (if (eql origin inspected-window)
          (observe (shape w) inspected-object)
          (update  (shape w) inspected-object (read-from-string
                                               (subseq (format nil "~a" message) 4))))))
  (apply 'change w origin message msg-args))

(defmethod ev-double-click
    ((w inspector-window) sender clicked button position)
  (when (typep clicked 'prop-button)
    (let* ((prop (prop-sym clicked))
           (setter (setter-name prop))
           (inspected-object (inspected-object w))
           (current-value (funcall prop inspected-object)))
      (when setter
        (multiple-value-bind (value set) (if (typep current-value 'boolean)
                                             (values (not current-value) t)
                                             (prompt-for-prop-value prop current-value))
          (when set
            (funcall setter inspected-object value)))))))

(defun setter-name (prop)
  (values (find-symbol (format nil "SET-~a" prop))))

(defun prompt-for-prop-value (prop current-value)
  (funcall (cond
             ((eql prop 'color) 'capi:prompt-for-color)
             ((eql prop 'background) 'capi:prompt-for-color)
             ((numberp current-value) 'capi:prompt-for-number)
             (t 'capi:prompt-for-value))
           (format nil "Vyber novou hodnotu vlastnosti ~a" prop)))

(defmethod check-shape ((w inspector-window) shape)
  (error "Cannot set shape of inspector-window"))

(defmethod inspected-window ((w inspector-window))
  (slot-value w 'inspected-window))

(defmethod do-set-inspected-window ((w inspector-window) win)
  (setf (slot-value w 'inspected-window) win)
  (when win
    (observe (shape w) win)
    (set-delegate win w)))

(defmethod check-wintype ((w inspector-window) win)
  (when win
    (let ((wintype (type-of win)))
      (unless (eql wintype 'inspected-window)
        (error (format nil "Recieved argument of type ~a instead of type inspected-window." wintype))))))

(defmethod set-inspected-window ((w inspector-window) win)
  (check-wintype w win)
  (send-with-change w 'do-set-inspected-window 'set-inspected-window win))

(defmethod inspected-object ((w inspector-window))
  (let ((inspected-window (inspected-window w)))
    (when inspected-window
      (or (selection inspected-window)
          inspected-window))))


#|

(setf prohlizec (make-instance 'inspector-window))

(setf w (make-instance 'inspected-window))

(setf pic (move (set-items (make-instance 'picture)
(list
(move (set-radius (set-filledp (make-instance 'circle) t) 40) 100 100)
(set-radius (set-filledp (make-instance 'circle) t) 60)
) ) 100 100))

(set-shape w pic)
(set-inspected-window prohlizec w)

|#
