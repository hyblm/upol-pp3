;; -*- mode: lisp; encoding: utf-8; -*-
(in-package "MICRO-GRAPHICS")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
|
|
| Knihovna MICRO-GRAPHICS
|
|
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pushnew '(*standard-output* . *standard-output*) 
         mp:*process-initial-bindings*
         :key 'car)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; pomůcky
;;

(defun %get-subset (set num)
  (loop for bit below (integer-length num)
        for elem in set
        when (logbitp bit num) collect elem))

#|
(%get-subset '(a b c d e) #b101)
|#

(defmacro do-subsets ((var set-form &optional result-form) &body body)
  (lw:rebinding (set-form)
    (lw:with-unique-names (num)
      `(let (,var)
         (loop for ,num below (expt 2 (length ,set-form))
               do (progn (setf ,var (%get-subset ,set-form ,num))
                    ,@body)
               finally (return ,result-form))))))

#|
(do-subsets (a (list 1 2 3 4))
  (print a))

(do-subsets (a nil)
  (print a))
|#

(defun map-subsets (function set)
  (let (res)
    (do-subsets (subset set (reverse res))
      (push (funcall function subset) res))))

(defun mouse-callbacks (what)
  (mapcan (lambda (but lcr)
            (map-subsets (lambda (subset)
                           `((,but ,what ,@subset) call-mouse-callback ,what ,lcr ,subset))
                         '(:shift :control :meta)))
          '(:button-1 :button-2 :button-3)
          '(:left :center :right)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; okna
;;

(defparameter *default-font-params* '(:family #+cocoa"Helvetica" #-cocoa"Arial" :size 11))

(defparameter *dummy-port* (capi:create-dummy-graphics-port))
(setf (gp:graphics-port-font *dummy-port*)
      (gp:find-best-font *dummy-port* (apply 'gp:make-font-description 
                                             *default-font-params*)))

(defclass abstract-window ()
  ((%filledp :initform nil :accessor filledp)
   (%closedp :initform nil :accessor closedp)
   (%mask :initform nil :accessor mask)))

(capi:define-interface mg-interface (capi:interface abstract-window)
  ((%callbacks :initform '() :accessor callbacks :initarg :callbacks)
   (%images :initform (make-hash-table :test #'equalp) :reader images)
   (%sounds :initform (make-hash-table :test #'equalp) :reader sounds)
   (%timers :initform '() :accessor timers)
   (%offscreen-windows :initform '() :accessor offscreen-windows))
  (:panes (pane capi:output-pane 
                :reader pane
                :draw-with-buffer t
                :display-callback 'call-display-callback
                :resize-callback 'pane-resize-callback
                :input-model (list* '(:motion call-mouse-callback :motion nil nil)
                                    '(:character call-char-callback)
                                    '((:key t :press) key-callback)
                                    (append (mouse-callbacks :press)
                                            (mouse-callbacks :release)
                                            (mouse-callbacks :second-press)
                                            (mouse-callbacks :motion)))))
  (:layouts 
   (main capi:column-layout '(pane) :default t))
  (:default-initargs :destroy-callback 'call-destroy-callback
                     :activate-callback 'call-activate-callback))

(defmethod pane-width ((intf mg-interface))
  (capi:simple-pane-visible-width (pane intf)))

(defmethod pane-height ((intf mg-interface))
  (capi:simple-pane-visible-height (pane intf)))

(defmethod pane ((pane capi:output-pane))
  pane)

(defmethod pane-font ((intf mg-interface))
  (gp:graphics-port-font (pane intf)))

(defmethod (setf pane-font) (value (intf mg-interface))
  (setf (gp:graphics-port-font (pane intf)) value))

(defmethod apply-in-p-p ((pane mg-interface) fun &rest args)
  (apply 'capi:apply-in-pane-process pane fun args))

(defmethod print-object ((w mg-interface) stream)
  (print-unreadable-object (w stream :type nil :identity t)
    (princ "MG-WINDOW" stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Zpětná volání
;;

;; CAPI volá callbacky vždy ve vlákně okna.
;; Na MacOS je ale spouštíme ve zvláštním procesu, aby šlo ladit (breakpointy).
;; S výjimkou překreslování, protože by okno blikalo.

(defvar *callback-lock* (mp:make-lock :name "MICRO-GRAPHICS-LOCK"))

#|
(defun call-back-cocoa (fun intf args output)
  (mp:with-lock (*callback-lock*)
    (let ((*standard-output* output))
      (apply fun intf args))))
|#

(defun call-back-cocoa (fun intf args)
  (mp:with-lock (*callback-lock*)
    (apply fun intf args)))

(defun call-back (pane-or-intf callback-type &rest args)
  (let* ((intf (capi:element-interface (pane pane-or-intf)))
         (fun (getf (callbacks intf) callback-type)))
    (when fun 
      (if (and (not (eql callback-type :display))
               (find :cocoa *features*))
          (let ((system:*sg-default-size* 64000))
            (mp:process-run-function "MICRO-GRAPHICS"
                                     '()
                                     'call-back-cocoa fun intf args))
        (apply fun intf args)))))

(defun pane-resize-callback (pane x y w h)
  (declare (ignore x y))
  (call-back pane :resize w h))

(defun call-display-callback (pane &rest args)
  (declare (ignore args))
  (call-back pane :display))

(defun call-destroy-callback (intf &rest args)
  (declare (ignore args))
  (call-back intf :destroy)
  (maphash (lambda (key val)
             (declare (ignore key))
             (capi:stop-sound val))
           (sounds intf))
  (dolist (tim (timers intf))
    (mp:unschedule-timer (mp-timer tim)))
  (dolist (off (offscreen-windows intf))
    (gp:destroy-pixmap-port (pane off))))

(defun call-activate-callback (intf actp)
  (call-back intf :activate actp))

(defun call-iconify-callback (intf iconifyp)
  (call-back intf :minimize iconifyp))

(defun call-mouse-callback (pane x y what button modifiers)
  (declare (ignore modifiers))
  (call-back pane
             (cdr (find what '((:press . :mouse-down) (:second-press . :double-click)
                               (:release . :mouse-up) (:motion . :mouse-move))
                        :key #'car))
             button x y))

(defun call-char-callback (pane x y char)
  (declare (ignore x y))
  (call-back pane :character char))

;;
;; Zpětná volání na klávesy při přednášce (prezentace)
;;

;; Indirekce, aby se předešlo informaci o nedefinované funkci, když není
;; :mk-present ve *features*.
(defvar *prompt-for-presentation-position-fun* 'cl-user::prompt-for-presentation-position)
(defvar *insert-from-presentation-fun* 'cl-user::insert-from-presentation)
(defvar *dec-presentation-pos-fun* 'cl-user::dec-presentation-pos)
(defvar *inc-presentation-pos-fun* 'cl-user::inc-presentation-pos)

(defun key-callback (w x y spec)
  (declare (ignore w x y))
  ;(format t "~%Key callback with args w: ~s, x: ~s, y: ~s, gesture-spec: ~s " w x y spec)
  (let ((key (system:gesture-spec-data spec)))
    (when (find :mk-present *features*)
      (case key
        (:F3 (funcall *prompt-for-presentation-position-fun*))
        (:F4 (funcall *insert-from-presentation-fun*))
        (:F5 (funcall *dec-presentation-pos-fun*))
        (:F6 (funcall *inc-presentation-pos-fun*))))
    ;;(format t "~%~s " key)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Vstupní body
;;

;; Všechny by měly kontrolovat správnost parametru window a zařídit, pokud je třeba,
;; spuštění ve správném vlákně.

(defun check-window (window fun &optional (type 'abstract-window))
  (unless (typep window type)
    (error "micro-graphics: funkce ~@[MG:~a ~]volána s nesprávným odkazem na okno (~s)" 
           fun window)))

;; používat na definici vstupních bodů s oknem. Přidává test na správnost okna.
(defmacro defmgfun (name (window &rest other-params) &body body)
  (let ((type 'abstract-window))
    (when (consp window)
      (setf type (second window)
            window (first window)))
    `(defun ,name (,window ,@other-params)
       (check-window ,window ',name ',type)
       ,@body)))

(defun display-window (&key (always-on-top-p t)
                            (height 210)
                            (width 297)
                            (title "micro-graphics window")
                            callbacks)
  (let ((result (make-instance 'mg-interface 
                               :title title
                               :height height
                               :width width
                               :window-styles `(:always-on-top ,always-on-top-p)
                               :callbacks callbacks)))
    (capi:display result)
    ;; Nastavime ručně, na nekterych platformach zrejme jinak drawing-state vraci
    ;; interni data misto nazvu barev:
    (set-param result :foreground :black)
    (set-param result :background :white)
    ;; Ve Windows bez nastavení fontu hlásí chybu při čtení (?)
    ;(set-param result :font *default-font-params*)
    (setf (gp:graphics-port-font (pane result))
          (gp:graphics-port-font *dummy-port*))
    result))

(defmethod %close-window ((window mg-interface))
  (apply-in-p-p window #'capi:quit-interface window))

(defmgfun close-window (window)
  (%close-window window)
  nil)

(defun trans-mg-mask (mg-mask)
  "Převod mg-masky na gp-masku"
  `(:path ,(loop for (x y . rest) = mg-mask then rest
                 for keyword = :move then :line
                 collect `(,keyword ,x ,y)
                 while rest)
    :fill-rule :even-odd))

#|
(trans-mg-mask '(1 2 3 4 5 6 7 8 9 10))
|#

(defmethod (setf mask) :before (value (intf abstract-window))
  (gp:set-graphics-state (pane intf) 
                         :mask (when value (trans-mg-mask value))))

(defmgfun get-param (window param)
  (let ((state (gp:get-graphics-state (pane window))))
    (ccase param
      (:thickness (gp:graphics-state-thickness state))
      (:foreground (gp:graphics-state-foreground state))
      (:background (gp:graphics-state-background state))
      (:transform (gp:graphics-state-transform state))
      (:mask (mask window))
      (:filledp (filledp window))
      (:closedp (closedp window))
      (:font (gp:font-description-attributes 
              (gp:font-description (pane-font window)))))))

(defmgfun set-param (window param value)
  (ccase param 
   (:filledp (setf (filledp window) value))
   (:closedp (setf (closedp window) value))
   (:mask (apply-in-p-p window #'(setf mask) value window))
   ((:thickness :foreground :background :transform) 
    (apply-in-p-p 
     window 
     'gp:set-graphics-state 
     (pane window) param value))
   (:font (apply-in-p-p
           window
           (lambda ()
             (setf (pane-font window) 
                   (gp:find-best-font (pane window)
                                      (apply 'gp:make-font-description value)))))))
  nil)

(defmgfun get-callback ((window mg-interface) callback-type)
  (getf (callbacks window) callback-type))

(defmgfun set-callback ((window mg-interface) callback-type function)
  (apply-in-p-p
   window
   (lambda ()
     (setf (getf (callbacks window) callback-type) 
           function)))
  #|(when (eql callback-name :display)
    (invalidate window))|#
  nil)

(defmgfun invalidate ((window mg-interface))
  (apply-in-p-p window #'gp:invalidate-rectangle (pane window))
  nil)

(defmgfun do-drawing ((w mg-interface) fun)
  (when (get-callback w :display)
    (error "Window ~s already has a display callback" w))
  (set-callback w :display (lambda (ww)
                             (set-callback ww :display nil)
                             (funcall fun)))
  (invalidate w))

(defmacro drawing (window &body body)
  `(do-drawing ,window (lambda ()
                         ,@body)))

#|

(drawing w (draw-circle 100 100 50))

|#

(defmgfun set-size ((window mg-interface) width height)
  (apply-in-p-p window 
                #'capi:set-top-level-interface-geometry 
                window :width width :height height)
  nil)

(defmgfun get-size ((window mg-interface))
  (multiple-value-bind (x y w h)
      (capi:top-level-interface-geometry window)
    (declare (ignore x y))
    (list w h)))

(defmgfun get-drawing-size (window)
  (list (pane-width window) (pane-height window)))

(defmgfun clear (window)
  (apply-in-p-p window #'gp:clear-graphics-port (pane window))
  nil)

(defmgfun draw-circle (window x y r)
  (apply-in-p-p 
   window
   ;; round: v OS X kreslí špatně při reálných číslech
   #'gp:draw-circle 
   (pane window) (round x) (round y) (round r) :filled (filledp window))
  nil)

(defmgfun draw-ellipse (window x y rx ry phi)
  (let ((s (sin phi))
        (c (cos phi)))
    (apply-in-p-p 
     window
     ;; round: v OS X kreslí špatně při reálných číslech
     #'gp:draw-ellipse 
       (pane window) 0 0 (round rx) (round ry)
       :transform (gp:make-transform  c  s (- s) c (round x) (round y))
       :filled (filledp window))
    nil))

(defmgfun draw-polygon (window points)
  (when points  ;Na Linuxu chyba, když je seznam prázdný.
    (apply-in-p-p 
     window 
     #'gp:draw-polygon 
     (pane window) points :closed (closedp window) :filled (filledp window))
    nil))

(defun string-paragraphs (string)
  (loop for old-position = 0 then (1+ position)
        for position = (position #\newline string :start old-position)
        for word = (subseq string old-position position)
        collect word
        while position))

(defun get-line-extent (string)
  (multiple-value-bind (l tp r b) (gp:get-string-extent *dummy-port* string)
    (list l tp r b)))

(defun get-string-extent (string)
  (let ((extents (mapcar (lambda (par)
                           (get-line-extent par))
                         (string-paragraphs string))))
    (list (reduce 'min extents :key 'first)
          (second (first extents))
          (reduce 'max extents :key 'third)
          (+ (fourth (first extents))
             (* (gp:get-font-height *dummy-port*) (1- (length extents)))))))

(defun %draw-string (window string x y angle)
  (let ((f-height (gp:get-font-height (pane window)))
        (c (cos angle))
        (s (sin angle)))
    (loop for line in (string-paragraphs string)
          for line-no from 0
          do (gp:draw-string (pane window) line 0 (* line-no f-height)
                             :transform (gp:make-transform  c  s (- s) c x y)
                             ;; kvůli offscreen-window
                             ;:font (pane-font window)
                             )))
  nil)

(defmgfun draw-string (window string x y &optional (phi 0))
  (apply-in-p-p
   window
   #'%draw-string
   window string x y phi)
  nil)

(defun path (relative-path)
  (merge-pathnames relative-path (or *load-pathname* *compile-file-pathname*)))

(defun get-image (file intf)
  (let ((result (gethash file (images intf))))
    (unless result
      (setf result (gp:load-image intf file))
      (setf (gethash file (images intf)) result))
    result))

(defun %draw-image (window file x y)
  (gp:draw-image (pane window) 
                 (get-image file (if (typep window 'mg-interface)
                                     window
                                   (interface window)))
                 x y))

(defmgfun draw-image (window file x y)
  (apply-in-p-p window
                #'%draw-image
                window file x y))

(defmgfun get-image-size ((window mg-interface) file)
  (let ((image (get-image file window)))
    (list (gp:image-width image)
          (gp:image-height image))))

(defun get-sound (file intf)
  (let ((result (gethash file (sounds intf))))
    (unless result
      (setf result (capi:load-sound file :owner intf))
      (setf (gethash file (sounds intf)) result))
    result))

(defun %play-sound (window file)
  (capi:play-sound (get-sound file window)))

(defmgfun play-sound ((window mg-interface) file)
  (apply-in-p-p window #'%play-sound window file))

(defun %stop-sound (window file)
  (capi:stop-sound (get-sound file window)))

(defmgfun stop-sound ((window mg-interface) file)
  (apply-in-p-p window #'%stop-sound window file))

(defun translate-color (object)
  (color:get-color-spec object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; časovače
;;

(defclass timer ()
  ((mp-timer :accessor mp-timer)))

(defmethod print-object ((w timer) stream)
  (print-unreadable-object (w stream :type nil :identity t)
    (princ "MG-TIMER" stream)))

(defun %timer-entry (window timer callback)
  (let ((seconds (funcall callback window timer)))
    (when seconds
      (mp:schedule-timer-relative (mp-timer timer) seconds))))

(defun timer-entry (window timer callback)
  (apply-in-p-p window
                '%timer-entry
                window
                timer
                callback))

(defmgfun add-timer-callback ((window mg-interface) seconds callback)
  (let ((tim (make-instance 'timer)))
    (setf (mp-timer tim)
          (mp:make-named-timer 
           "MG-TIMER" 
           'timer-entry window tim callback))
    (apply-in-p-p window
                  (lambda (tim w)
                    (push tim (timers w)))
                  tim
                  window)
    (mp:schedule-timer-relative (mp-timer tim) seconds)
    tim))
    

(defmgfun remove-timer-callback ((window mg-interface) timer)
  (apply-in-p-p window
                (lambda (tim w)
                  (mp:unschedule-timer (mp-timer timer))
                  (setf (timers w) (remove tim (timers w))))
                timer
                window)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; offscreen-window
;;;

(defclass mg-offscreen (abstract-window)
  ((%bitmap :reader pane)
   (%intf :reader interface)))

(defmethod print-object ((w mg-offscreen) stream)
  (print-unreadable-object (w stream :type nil :identity t)
    (princ "MG-OFF-WINDOW" stream)))

(defmethod pane-font ((off mg-offscreen))
  (pane-font (interface off)))

(defmethod (setf pane-font) (value (off mg-offscreen))
  (setf (pane-font (interface off)) value))

(defmethod apply-in-p-p ((w mg-offscreen) fun &rest args)
  (apply 'apply-in-p-p (interface w) fun args))

(defmethod pane-width ((w mg-offscreen))
  (pane-width (interface w)))

(defmethod pane-height ((w mg-offscreen))
  (pane-height (interface w)))

(defun offscreen-init (window off)
  (setf (slot-value off '%bitmap) 
        (gp:create-pixmap-port (pane window) (pane-width window) (pane-height window))
        (slot-value off '%intf)
        window)
  (push off (offscreen-windows window)))

(defmgfun make-offscreen-window ((window mg-interface))
  (let ((result (make-instance 'mg-offscreen)))
    (apply-in-p-p window 'offscreen-init window result)
    result))

(defmethod %close-window ((off mg-offscreen))
  (let ((window (interface off)))
    (apply-in-p-p window
                  (lambda ()
                    (gp:destroy-pixmap-port (pane off))
                    (setf (offscreen-windows window)
                          (remove off (offscreen-windows window)))))
    nil))

(defmgfun draw-offscreen-window (window off)
  (apply-in-p-p window
                'gp:copy-pixels
                (pane window)
                (pane off)
                0 0 (pane-width window) (pane-height window) 0 0)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; hit-testing
;;;

(defun scalar-product (v1 v2)
  (apply '+ (mapcar '* v1 v2)))

(defun scalar-mult (k v)
  (mapcar (lambda (x) (* k x))
          v))

(defun vec-+ (v1 &rest vectors)
  (apply 'mapcar '+ v1 vectors))

(defun vec-- (v1 &rest vectors)
  (apply 'mapcar '- v1 vectors))

(defun vec-= (v1 v2)
  (every '= v1 v2))

(defun vec-sq-len (v)
  (scalar-product v v))

(defun vec-near-p (v1 v2 tolerance)
  (<= (vec-sq-len (vec-- v1 v2))
      (expt tolerance 2)))

(defun pt-in-seg-p (pt x1 x2 tolerance)
  "Zjisti, zda je bod pt na usecce [x1 x2]."
  (let* ((u (vec-- x2 x1))
         (v (vec-- x1 pt))
         (uu (scalar-product u u)))
    (if (zerop uu)
        (vec-near-p pt x1 tolerance)
      (let ((k (- (/ (scalar-product u v) uu))))
        (and (<= 0 k 1)
             (vec-near-p pt (vec-+ x1 (scalar-mult k u)) tolerance))))))

(defun point-in-segs-p (pt tolerance &optional pt1 pt2 &rest points)
  (and pt1 
       pt2
       (or (pt-in-seg-p pt pt1 pt2 tolerance)
           (apply 'point-in-segs-p pt tolerance pt2 points))))

(defun vert-between-p (pt pt1 pt2)
  (let ((pty (second pt))
        (pt1y (second pt1))
        (pt2y (second pt2)))
    (declare (number pty pt1y pt2y))
    (declare (optimize (speed 3) (safety 0)))
    (or (< pt1y pty pt2y)
        (> pt1y pty pt2y)
        ;;u mensiho z pt1y, pt2y umoznime i rovnost
        (and (/= pt1y pt2y)
             (= (min pt1y pt2y) pty)))))

(defun horiz-right-p (pt pt1 pt2)
  (destructuring-bind (ptx pty pt1x pt1y pt2x pt2y) (append pt pt1 pt2)
    (< (+ (* (- pt1x pt2x) 
             (/ (- pty pt2y)
                (- pt1y pt2y)))
          pt2x)
       ptx)))

(defun intersects-p (pt pt1 pt2)
  (and (vert-between-p pt pt1 pt2)
       (horiz-right-p pt pt1 pt2)))

(defun count-intersections (pt &optional pt1 pt2 &rest points)
  (if (and pt1 pt2)
       (+ (if (intersects-p pt pt1 pt2) 1 0)
          (apply 'count-intersections pt pt2 points))
    0))

(defun point-in-poly-p (pt points)
  (oddp (apply 'count-intersections pt points)))

(defun points-to-lists (points closedp filledp)
  (let ((res (loop for (x y) on points by 'cddr
                   collect (list x y))))
    (when (or closedp filledp)
      (setf res (append (last res) res)))
    res))

#|
(points-to-lists '(1 2 3 4 5 6 7 8 9 10))
|#

(defun point-in-polygon-p (x y closedp filledp thickness points)
  (if filledp
      (point-in-poly-p (list x y) (points-to-lists points closedp filledp))
    (apply 'point-in-segs-p (list x y) thickness (points-to-lists points closedp filledp))))

#|

;; Většina příkladů přestala fungovat v Mac OS. Kreslení by se muselo přenést do callbacku.

(defun paint-flag (w)
    (mg:set-param w :background :skyblue)
    (mg:set-param w :foreground :white)
    (mg:clear w)
    (mg:set-param w :filledp t)
    (mg:set-param w :closedp t)
    (mg:draw-polygon w '(20 20 200 20 200 140 20 140))
    (mg:set-param w :foreground :black)
    (mg:set-param w :filledp nil)
    (mg:draw-polygon w '(20 20 200 20 200 140 20 140))
    (mg:set-param w :filledp t)
    (mg:set-param w :foreground :red)
    (mg:draw-circle w 110 80 30))

(defun display-flag ()
  (mg:display-window :callbacks (list :mouse-down (lambda (w butt x y)
                                                    (declare (ignore w))
                                                    (format t "~%:mouse-down ~s ~s ~s" butt x y))
                                      :mouse-up (lambda (w butt x y)
                                                  (declare (ignore w))
                                                  (format t "~%:mouse-up ~s ~s ~s" butt x y))
                                      :double-click (lambda (w butt x y)
                                                      (declare (ignore w))
                                                      (format t "~%:double-click ~s ~s ~s" butt x y))
                                      :mouse-move (lambda (w butt x y)
                                                    (declare (ignore w))
                                                    (format t "~%:mouse-move ~s ~s ~s" butt x y))
                                      :character (lambda (w char)
                                                   (declare (ignore w))
                                                   (format t "~%:character ~s" char))
                                      :resize (lambda (w width height)
                                                (declare (ignore w))
                                                (format t "~%resize... ~s ~s" width height))
                                      :destroy (lambda (w)
                                                 (format t "~%Window ~s destroyed" w))
                                      :activate (lambda (w actp)
                                                  (if actp
                                                      (format t "~%Window ~s activated" w)
                                                    (format t "~%Window ~s deactivated" w)))
                                      :minimize (lambda (w minp)
                                                  (if minp
                                                      (format t "~%Window ~s minimized" w)
                                                    (format t "~%Window ~s de-minimized" w))))))

(setf w (display-flag))
(callbacks w)
(set-callback w :display (lambda (w) (paint-flag w)))

|#

#|
(setf w (mg:display-window))
(set-param w :mask '(60 100 160 100 160 200 60 200))
(get-param w :mask)
(set-param w :filledp t)
(set-param w :foreground :red)
(set-param w :thickness 3)
(mg:draw-ellipse w 148 105 40 60 0)
(mg:draw-ellipse w 148 105 40 60 (/ pi 6))
(mg:draw-ellipse w 148 105 40 60 (/ pi 3))
(mg:draw-ellipse w 148 105 40 60 (/ pi 2))
(mg:draw-ellipse w 148 105 40 60 (* pi 2/3))
(mg:draw-ellipse w 148 105 40 60 (* pi 5/6))
(mg:draw-circle w 148 105 20)
(mg:draw-circle w 148 105 200)

(set-param w :mask nil)
|#

#|
(setf w (mg:display-window))
(mg:set-param w :transform '(2 0 0 1 0 0))
(mg:draw-circle w 50 50 20)
|#

#|
(setf w (mg:display-window :title "Title"))
(get-size w)
(set-size w 500 500)
(get-drawing-size w)

(set-callback w :display (lambda (w)
                           (draw-string w (format nil "Řádek 1~%Řádek 2~%Řádek 3") 20 20)))
(get-string-extent (format nil "Řádek 1~%Řádek 2~%Řádek 3"))

(set-callback w :display
              (lambda (w)
                (let ((str (format nil "Dlouhý předlouhý řádek 1~%Kratší řádek 2~%Opravdu úplně nejdelší řádek 3")))
                  (draw-string w str 20 20)
                  (destructuring-bind (l top r b)
                      (get-string-extent str)
                    (incf l 20)
                    (incf top 20)
                    (incf r 20)
                    (incf b 20)
                    (draw-polygon w (list l top r top r b l b l top))))))


|#

#|
(setf w (mg:display-window))
(setf file1 (capi:prompt-for-file "Otevřít obrázek"))
(setf file2 (capi:prompt-for-file "Otevřít jiný obrázek"))

;; Tohle na MacOS nefunguje mimo :display callback.
;; takže následující dva řádky na MacOS nezkoušet
(draw-image w file1 10 10)
(draw-image w file2 100 100)

(set-callback w :display (lambda (w)
                           (draw-image w file2 10 10)))

(set-callback w :display (lambda (w)
                           (draw-image w file1 10 10)))

(invalidate w)

(get-image-size w file1)

|#

#|
(setf w (mg:display-window))
(setf file1 (capi:prompt-for-file "Otevřít zvuk"))
(setf file2 (capi:prompt-for-file "Otevřít jiný zvuk"))
(play-sound w file1)
(play-sound w file2)
(stop-sound w file1)
(stop-sound w file2)

|#

#|
(path "test.txt")
|#

#|
;; test rotace textu

(setf w (mg:display-window))
(set-param w :foreground :orange)
(dotimes (i 12)
  (mg::draw-string w "Sluneční paprsek
na dva řádky" 148 105 (* pi (/ i 6))))
|#

#|
;; testy fontu
(setf w (mg:display-window))
(get-param w :font)
(dotimes (i 30)
  (mg:set-param w :font `(:size ,(+ 5 i)))
  (mg:draw-string w "Sluneční paprsek" 300 200 (* pi (/ i 15))))

(mg:set-param w :font '(:size 16 :slant :italic :family "Times"))
(mg:draw-string w "K Amále záhadná banda házela mák." 20 20)

(mg:set-param w :font '())
|#

#|
;; test časovačů
(setf w (mg:display-window))
(set-callback w :mouse-move (lambda (w butt x y)
                              (set-param w :filledp nil)
                              (set-param w :foreground :skyblue)
                              (draw-circle w x y 15)))

(setf tim1
      (add-timer-callback w 2 (let ((i 0))
                                (lambda (w tim)
                                  (set-param w :foreground :orange)
                                  (draw-string w "Sluneční paprsek" 148 105 (* pi (/ i 6)))
                                  (incf i)
                                  (when (<= i 9) 2)))))

(setf tim2
      (add-timer-callback w 1.1 (lambda (w tim)
                                  (set-param w :foreground :red)
                                  (set-param w :filledp t)
                                  (draw-circle w (random 300) (random 200) (1+ (random 9)))
                                  1.1)))

(remove-timer-callback w tim1)
(remove-timer-callback w tim2)
(set-callback w :mouse-move nil)
|#

#|
;; offscreen window

(setf w (display-window))

(setf off (make-offscreen-window w))

(paint-flag off)

;; Toto nefunguje v Cocoa (MacOS)
(draw-offscreen-window w off)

;; V Cocoa je třeba použít toto:
(set-callback w :display (lambda (w)
                           (draw-offscreen-window w off)))

;; Další testy:
;; V Cocoa je vždy třeba okno donutit k překreslení
;; jinde stačí zavolat
(draw-offscreen-window w off)

(invalidate w)

(set-param off :foreground :orange)
(dotimes (i 12)
  (mg::draw-string off "Sluneční paprsek
na dva řádky" 148 105 (* pi (/ i 6))))

(set-param off :filledp t)
(set-param off :foreground :red)
(set-param off :thickness 3)
(mg:draw-ellipse off 148 105 40 60 0)
(mg:draw-ellipse off 148 105 40 60 (/ pi 6))
(mg:draw-ellipse off 148 105 40 60 (/ pi 3))
(mg:draw-ellipse off 148 105 40 60 (/ pi 2))
(mg:draw-ellipse off 148 105 40 60 (* pi 2/3))
(mg:draw-ellipse off 148 105 40 60 (* pi 5/6))

;; A libovolné kreslení s maskou:
(set-param off :mask '(60 100 160 100 160 200 60 200))

(mg:set-param off :font '(:size 16 :slant :italic :family "Times"))
(mg:draw-string off "K Amále záhadná banda házela mák." 20 20)
|#

#|
;; Rozsáhlejší test: "planetky" 

(setf w (display-window :height 500 :width 700))

(defun draw-star (window)
  (let ((size (get-drawing-size window)))
    (set-param window 
               :foreground (color:make-rgb (+ 0.7 (random 0.3))
                                           (+ 0.7 (random 0.3))
                                           (+ 0.7 (random 0.3))))
    (set-param window :filledp t)
    (draw-circle window 
                 (random (first size))
                 (random (second size))
                 (random 2.0))))

(defun draw-stars (window count)
  (set-param window :background :black)
  (clear window)
  (dotimes (i count)
    (draw-star window)))

(draw-stars w 300)

(defvar *sun-radius* 15)

(defun draw-sun (window)
  (let ((size (get-drawing-size window)))
    (set-param window :foreground :orange)
    (set-param window :filledp t)
    (draw-circle window
                 (* 0.5 (first size))
                 (* 0.5 (second size))
                 *sun-radius*)))

(draw-sun w)

(defun dist (x1 y1 x2 y2)
  (sqrt (+ (expt (- x1 x2) 2)
           (expt (- y1 y2) 2))))

(defvar *grav-const* 0.3)

;; Vrátí funkci pro kreslení planety s počátečním umístěním (x y)
;; ostatní parametry planety (poloměr, barva, poč. rychlost)
;; jsou náhodné
;; Výslednou funkci lze periodicky volat s oknem jako parametrem,
;; vykreslí planetu v aktuálním umístění. (V rozporu s fyzikálními
;; zákony, ale to snad nevadí.)
(defun make-planet (x y)
  (let (last-time
        (radius (+ 5 (random 5)))
        (velocity-x (- (random 200) 100)) 
        (velocity-y (- (random 200) 100))
        (color (color:make-rgb (+ 0.5 (random 0.5))
                               (+ 0.5 (random 0.5))
                               (+ 0.5 (random 0.5)))))
    (lambda (win)
      (let ((now (get-internal-real-time))
            (delta) ;v sekundách
            (center-x (* 0.5 (first (get-drawing-size win))))
            (center-y (* 0.5 (second (get-drawing-size win))))
            (distance))
        (setf delta (min (if last-time 
                             (* (/ 1.0 internal-time-units-per-second) (- now last-time))
                           0.1)
                         0.1)
              last-time now)
        (setf distance (dist x y center-x center-y))
        (set-param win :foreground color)
        (set-param win :filledp t)
        (draw-circle win x y radius)
        (unless (<= distance (+ *sun-radius* radius)) ;nabourali jsme slunce
          (incf x (* velocity-x delta))
          (incf y (* velocity-y delta))
          (incf velocity-x (* delta *grav-const* (- center-x x)))
          (incf velocity-y (* delta *grav-const* (- center-y y)))
          t)))))

;; Jednoduchý test:
(setf timer (add-timer-callback w 0.1 (let ((fun (make-planet 100 100)))
                                        (lambda (win timer)
                                          (declare (ignore timer))
                                          (when (funcall fun win) 0.1)))))

(remove-timer-callback w timer)

(draw-stars w 300)
(draw-sun w)

;; Pro jednoduchost globální proměnná, ale tím pádem nemůžeme
;; mít víc oken
(defvar *planets*)
(setf *planets* '())

;; Jeden krok kreslení planet do okna win.
;; mělo by se periodicky volat
(defun draw-planets-step (win)
  (let (new-planets)
    (dolist (planet *planets*)
      (when (funcall planet win)
        (push planet new-planets)))
    (setf *planets* new-planets)))

(defun timer-callback-1 (win timer)
  (declare (ignore timer))
  (draw-planets-step win)
  0.03)

(setf timer (add-timer-callback w 0.03 'timer-callback-1))

(defun mouse-down-callback (window button x y)
  (declare (ignore window))
  (when (eql button :left)
    (push (make-planet x y)
          *planets*)))

(set-callback w :mouse-down 'mouse-down-callback)

;; ... a můžeme klikat.

(setf *planets* '())

(remove-timer-callback w timer)

;; Teď se pustíme do překreslování okna. Řešení mezi Windows a Cocoa (MacOS) se liší.
;; Jak je to v Linuxu, bohužel nevím.
;; Na MacOS voláme invalidate okna (jinak to nejde), jinde rovnou kreslíme, protože 
;; jinak by okno blikalo
(defun timer-callback (win timer)
  (declare (ignore timer))
  ;; Následují direktivy, které vyberou výraz k vyhodnocení podle toho, zda jsme na Cocoa:
  #+cocoa(invalidate win)
  #-cocoa(let ((clbck (get-callback win :display)))
           (when clbck (funcall clbck win)))
  0.03)

(set-callback w :display 'draw-planets-step)
(setf timer (add-timer-callback w 0.03 'timer-callback))

;; A teď s hvězdičkami:

(defun make-display-callback-1 ()
  (let (off)
    (lambda (window)
      ;; Hvězdy a slunce vykreslíme jen poprvé:
      (unless off 
        (setf off (make-offscreen-window window))
        (draw-stars off 300)
        (draw-sun off))
      (draw-offscreen-window window off)
      (draw-planets-step window))))

(set-callback w :display (make-display-callback-1))

(defun planets-demo-1 (&optional (initial-count 0))
  (let (w)
    (setf *planets* '())
    (dotimes (i initial-count)
      (push (make-planet (random 700) (random 500)) *planets*))
    (setf w (display-window :width 700 :height 500))
    (set-callback w :display (make-display-callback-1))
    (set-callback w :mouse-down 'mouse-down-callback)
    (add-timer-callback w 0.03 'timer-callback)))

(planets-demo-1)
;; Zátěžový test:
(planets-demo-1 500)


|#