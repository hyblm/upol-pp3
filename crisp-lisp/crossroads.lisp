;; svetla semafour jsou instance tridy `light' ze souboru `04_light.lisp'
;;
;; semafor ma nastavenou vlastnost `semaphore-type' s moznimy hodnotami `:pedestrian' a `:vehicle' ... dalsi dobrovolne
;;
;; instance tridy semafor se vykresluji jako jednoduchy obrazek semaforu (spravny pocet kolecek v obdelniku)
;;
;; semafor se muze nachaze v jedne z nekolika fazi, podle sveho typu
;; napr. semafor pro vozidla ma ctyri faze
;;  - cervena
;;  - cervena + oranzova
;;  - zelena
;;  - oranzova
;; aktualni barvy svetel semaforu musi odpovidat jeho fazi.
;;
;; Cislo faze je ulozene v nastavitelne vlastnosti `semaphore-phase'
;;
;; Pocet fazi bude ulozeny ve vlastnosti `phase-count'
;; - faze se cisluji od nuly
;;
;; pro prechod ka nasledujici fazi semafor implementuje metodu `next-phase'.
;;
(defclass semaphore (abstract-picture)
  )

;; vlastnost `items' bude nastavitelna uzivatelem.
;; muze obsahovat libovolne graficke objekty, z nichz nektere mohou byt semafory
;;
;; trida definuje vlastnost `semaphores' (read-only), ktera obsahuje seznam vsech semaforu v krizovatce
;;
;; krizovatka se, podobne jako semafor, muze nachazet v ruznych fazich, ktere se opet prepinaji
;; zpravou `next-phse' a jsou ulozeny v nastavitelne vlastnosti `crossroads-phase'.
;; pocet fazi je ulozen ve vlastnosti `phase-count'.
;;
;; faze krizovatky urcuji, v jakych fazi jsou jeji semafory. to je zadano nastavitelnou vlastnosti `program',
;; ktera obsahuje program semaforu, to je seznam seznamu
;;  - jeho delka udava pocet fazi
;;  - i-ty podseznam programu urcuje stav krizovatky v jeji i-te fazi.
;;  - kazdy podseznam ma delku rovnou poctu semaforu v krizovatce a pro kazdy semafor obsahuje cislo jeho faze
;;
;;    EG: pro krizovatku o trech semaforech a programem `((0 0 0) (0 1 0) (0 2 1))'
;;        plati, ze ve fazi 2, je jeji prvni semafor ve fazi 0, druhy ve fazi 2 a treti ve fazi 1
;;
(defclass crossroads (picture)
  )
