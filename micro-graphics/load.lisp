(in-package "CL-USER")

(set-default-character-element-type 'simple-char)

(defsystem micro-graphics ()
  :members ("package" "micro-graphics")
  :rules ((:compile :all 
           (:requires (:load :previous)))))

(compile-system 'micro-graphics :load t)

#|
(compile-system 'micro-graphics :load t :force-p t)
|#