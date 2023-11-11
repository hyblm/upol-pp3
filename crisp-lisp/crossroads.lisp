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

`semaphore-type':   typ semaforu, nabyva hodnot `:pedestrian' nebo `:vehicle'
`semaphore-phase':  cislo faze semaforu, cislovani fazi zacina od 0, faze se prepinaji zpravou `next-phase'
`phase-count':      pocet fazi semaforu

ZPRÁVY

`next-phase':       zprava na prepinani faze, inkrementuje `semaphore-phase' po `phase-count', po dosazeni `phase-count' reset na 0

|#
(defclass semaphore (abstract-picture)
  ((semaphore-type  :initform :pedestrian)
   (semaphore-phase :initform 0)
   (phase-count     :initform 1)))

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
