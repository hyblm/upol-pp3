(defun make-ghost (color scale-coeff)
  (let ((width (* scale-coeff 150))
        (height (* scale-coeff 200)))
    (make-instance 'circle)
    (set-filledp
     (set-color
      (set-radius
       (make-instance 'circle)
       (/ width 2))
      color)
     t)
    ))

(defun make-ghost-random (&optional (colors '(:green)))
  (let ((color (nth (random (length colors)) colors))
        (scale-coeff (+ 0.25 (random 0.25)))
        (angle (+ -0.2 (random 0.4)))
        (dx (random 750))
        (dy (random 500)))
    (move
     (rotate (make-ghost color scale-coeff) angle (make-instance 'point))
     dx dy)
    ))

(defun make-ghosts-random (count colors &optional (ir '()))
  (if (zerop count)
      ir
      (make-ghosts-random
       (1- count)
       colors
       (cons (make-ghost-random colors) ir))))

(defun display-halloween-window (ghost-count)
  (let ((w (make-instance 'window))
        (colors '(:red           ; Blinky
                  :magenta       ; Pinky
                  :cyan          ; Inky
                  :orange        ; Clyde
                  :purple        ; Sue
                  :lightgray     ; Dinky - needs a hat
                  :darkgoldenrod ; Tim
                  :green         ; Funky
                  :yellow        ; Kinky
                  :gray          ; Orson
                  )))
    (set-background w :black)
    (set-shape w (set-items
                  (make-instance 'picture)
                  (make-ghosts-random ghost-count colors)))
    ;; cannot redraw the window before it is fully displayed
    (sleep 0.1)
    (redraw w)))
