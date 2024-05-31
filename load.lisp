;; 08.load

#|
Načtením tohoto souboru načtete všechny soubory potřebné k práci 
s knihovnou OMG ve verzi z 8. přednášky včetně příkladů.

Organizace adresářů: v adresáři, kde je tento 
soubor, musí být 

soubor "lib.lisp"

a následující adresáře:

"micro-graphics" s knihovnou micro-graphics
"Examples" se souborem "05_bounds.lisp"
"Examples" s "07_click-circle.lisp"
"Examples" se všemi příklady k přednášce 8.

Pokud máte už načtenou jinou verzi knihovny, nejprve ukončete LispWorks.
|#

(in-package "CL-USER")

(set-default-character-element-type 'simple-char)

(defsystem pp3-08 ()
  :members ("micro-graphics/load" "lib" "Examples/05_bounds" "Examples/07_click-circle.lisp"
            "Examples/08_text-shape" "Examples/08_button" "Examples/08_cwa"
            "Examples/08_cw2a" "Examples/08_polygon-canvas" "Examples/08_polygon-editor")
  :rules ((:compile :all 
           (:requires (:load :previous)))))

(compile-system 'pp3-08 :load t)
