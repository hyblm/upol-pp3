;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; crossroads.lisp -  - druhy ukol :: Autor: Matyas Hybl
;;;;

#|

DOKUMENTACE
-----------

Instance tridy semafor se vykresluji jako jednoduchy obrazek semaforu (prislusny poce svetel v obdelniku).
Semafor se muze nachaze v jedne z nekolika fazi, podle sveho typu
napr. semafor pro vozidla ma ctyri faze
- cervena
- cervena + oranzova
- zelena
- oranzova
aktualni barvy svetel semaforu musi odpovidat jeho fazi.

VLASTNOSTI

`semaphore-type':      typ semaforu, nabyva hodnot `:pedestrian' nebo `:vehicle'
`semaphore-phase':     cislo faze semaforu, cislovani fazi zacina od 0, faze se prepinaji zpravou `next-phase'
`phase-count':         pocet fazi semaforu
`gap-between-lights':  vzdalenost mezi jednotlivymi svetli semaforu, a zaroven mezi svetly a okrajem semaforu
`lights':              seznam svetel semaforu, nastavuje se pouze pri zmene `semaphore-type'

ZPRÁVY

`next-phase':          zprava na prepinani faze, inkrementuje `semaphore-phase' po `phase-count', po dosazeni `phase-count' reset na 0

|#
(defclass semaphore (abstract-picture)
  ((semaphore-type   :initform :pedestrian)
   (semaphore-phase  :initform 0)
   (phase-count      :initform 1)
   (items            :initform '())
   (gap              :initform 0)
   (width            :initform 0)
   ))

(defmethod next-phase ((sem semaphore))
  (let* ((type (slot-value sem 'semaphore-type))
         (phase (slot-value sem 'semaphore-phase))
         (program (nth phase (case type
                               (:pedestrian +pedestrian-program+)
                               (:vehicle    +vehicle-program+)))))
    (mapcar (lambda (should-toggle light)
              (when should-toggle
                (toggle light)))
            program (slot-value sem 'items))
    (if (< phase (1- (slot-value sem 'phase-count)))
        (incf (slot-value sem 'semaphore-phase))
        (setf (slot-value sem 'semaphore-phase) 0))
    )
  sem)

(defmethod initialize-instance ((sem semaphore) &key (type :pedestrian) (width 80) (gap 5))
  (call-next-method)
  (check-semaphore-type sem type)
  (setf (slot-value sem 'gap) gap
        (slot-value sem 'width) width
        (slot-value sem 'semaphore-type) type
        (slot-value sem 'phase-count) (case type
                                        (:pedestrian (length +pedestrian-program+))
                                        (:vehicle    (length +vehicle-program+)))
        (slot-value sem 'items) (copy-list (case type
                                             (:pedestrian +default-pedestrian-lights+)
                                             (:vehicle    +default-vehicle-lights+))))
  (send-to-items sem 'set-radius (/ (- width gap gap) 2))
  (mapcar (lambda (offset light) (move light 0 offset))
          (loop :for i :below (length (slot-value sem 'items)) :collect (* i (- width gap)))
          (slot-value sem 'items))
  (setf (slot-value sem 'items)
        (append
         (slot-value sem 'items)
         (list (let* ((num-lights (length (slot-value sem 'items)))
                      (height     (- (* width num-lights) (* gap (1- num-lights))))
                      (x-offset (/ width 2))
                      )
                 (set-items (set-filledp (make-instance 'polygon) t)
                            (list (move (make-instance 'point) (- x-offset) (- x-offset))
                                  (move (make-instance 'point)    x-offset  (- x-offset))
                                  (move (make-instance 'point)    x-offset  (- height x-offset))
                                  (move (make-instance 'point) (- x-offset) (- height x-offset))
                                  )))
               ))))

(defmethod check-semaphore-type ((sem semaphore) type)
  (unless (or (eql type :pedestrian)
              (eql type :vehicle))
    (error "Semaphore of given type isn't implemented")))

(defmethod check-item ((sem semaphore) item)
  (unless (typep item 'light)
    (error "Semaphore can only containt lights")))


#|
i-ty prvek daneho seznamu reprezentuje, kterym svetlum semaforu poslat zpravu `toggle',
aby se semafor spravne posunul na pristi fazi
|#
(defconstant +pedestrian-program+ '((t t) (t t)))
(defconstant +vehicle-program+    '((nil t nil) (t t t) (nil t t) (t t nil)))
(defconstant +radius+ 30)

;; Pedestrian semaphore lights in phase 0
(defconstant +default-pedestrian-lights+
  (list (turn-on  (set-on-color (make-instance 'light) :red))
        (turn-off (set-on-color (make-instance 'light) :green))))

;; Vehicle semaphore lights in phase 0
(defconstant +default-vehicle-lights+
  (list (turn-on  (set-on-color (make-instance 'light) :red))
        (turn-off (set-on-color (make-instance 'light) :orange))
        (turn-off (set-on-color (make-instance 'light) :green))))

#|

DOKUMENTACE
-----------

VLASTNOSTI

`items':             obsahuje libovolne graficke objekty, nektere z nich mohou byt semafory
`semaphores':        obsahuje seznam vsech semaforu v krizovatce, read-only
`crossroads-phase':  aktualni faze krizovatky, prepinani na dalsi fazi zarizuje zprava `next-phase'
`phase-count':       pocet fazi krizovatky
`program':           seznam, jehoz i-ty prvek reprezentuje i-tou fazi krizovatky,
jednotlive faze jsou take seznamy, jejichz i-ty prvek
je cislo n reprezentujici n-tou fazi i-teho semaforu
---
EG: pro krizovatku o trech semaforech a programem `((0 0 0) (0 1 0) (0 2 1))'
plati, ze ve fazi 2, je jeji prvni semafor ve fazi 0, druhy ve fazi 2 a treti ve fazi 1
---

ZPRÁVY

`next-phase':        zprava na prepinani faze, inkrementuje `crossroads-phase' po `phase-count',
po dosazeni `phase-count' reset na 0.
Musi take upravit faze vsech semaforu podle `program'u

|#
(defclass crossroads (picture)
  (
   (items            :initform '())
   (semaphores       :initform '())
   (crossroads-phase :initform 0)
   (phase-count      :initform 0)
   (program          :initform '())))



#| TESTS |#
()
