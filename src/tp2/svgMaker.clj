(ns tp2.svgMaker  (:gen-class))
(require '[tp2.svgMaker :as svgMaker])

(defn a
  [angulo x]
  (if (= x "F")
    " l 10 0"
    (if (= x "+")
      (str " l " angulo " " angulo) 
      (if (= x "-") 
        (str " l -" angulo " -" angulo)
        ""
        )
      )
    )
  )

(defn lineInLine [expresions angulo] 
   (map (partial a angulo) expresions)
)

(defn lineInLine_Testa [ formula angulo pila ] ;; pila inicialmente será (0 0 0)
  ( if (not (empty? formula)) ( 
      if (= (get (first formula) 0) (char 91)) (    ;; Esto es un gran if else if else if else ...
        ;; Agregamos a la pila el mismo elemento que tenemos antes
        lineInLine_Testa (next formula) angulo (conj pila (peek pila))  
      ) ( 
      if (= (first formula) "]") (
         str "M " 
          ((peek (pop pila)) :x) " " 
          ((peek (pop pila)) :y) " " 
          (lineInLine_Testa  (next formula) angulo (pop pila) ) " "
        ;; Quitar tortuga y escribir linea con pluma levantada para volver a punto inicial 
      ) (
      if (= (first formula) "+") (
         lineInLine_Testa (next formula) angulo (conj (pop pila) {
          :x ((peek pila) :x), 
          :y ((peek pila) :y), 
          :a (+ ((peek pila) :a) angulo) 
          } )  
        ;; Siguiente elemento en formula, quitamos elemento con angulo anterior y agregamos mismo elemento + angulo
      ) (
      if (= (first formula) "-") (
         lineInLine_Testa (next formula) angulo (conj (pop pila) {
          :x ((peek pila) :x), 
          :y ((peek pila) :y), 
          :a (- ((peek pila) :a) angulo) 
          } ) 
        ;; Siguiente elemento en formula, quitamos elemento con angulo anterior y agregamos mismo elemento - angulo
      ) (       
        ;; Else
         str "L " 
          (+ ((peek pila) :x) (* (Math/cos ((peek pila) :a)) 10)) " " 
          (+ ((peek pila) :y) (* (Math/sin ((peek pila) :a)) -10)) " " 
          ( lineInLine_Testa  (next formula) angulo (conj (pop pila) { 
            :x (+ ((peek pila) :x) (* (Math/cos ((peek pila) :a)) 10)), 
            :y (+ ((peek pila) :y) (* (Math/sin ((peek pila) :a)) -10)), 
            :a (get (peek pila) :a)
            })
          ) " "  
        
      ))))
    ) 
    ( str nil ) 
  )
)

(defn procesar_pila [angulo pila elemento] (
  if (= elemento "[") (conj pila (peek pila)) (
  if (= elemento "]") (pop pila) (
  if (= elemento "+") (conj (pop pila) {
    :x ((peek pila) :x), 
    :y ((peek pila) :y), 
    :a (+ ((peek pila) :a) angulo) 
    } ) (
  if (= elemento "-") (conj (pop pila) {
    :x ((peek pila) :x), 
    :y ((peek pila) :y), 
    :a (+ ((peek pila) :a) angulo) 
    } ) (
  conj (pop pila) { 
    :x (+ ((peek pila) :x) (* (Math/cos ((peek pila) :a)) 10)), 
    :y (+ ((peek pila) :y) (* (Math/sin ((peek pila) :a)) -10)), 
    :a (get (peek pila) :a)
    } 
))))))

(defn procesar_salida [pila elemento] (
  if (= elemento "]") (
    str "M " 
      ((peek (pop pila)) :x) " " 
      ((peek (pop pila)) :y) " " 
  ) (
    str "L "
    (+ ((peek pila) :x) (* (Math/cos ((peek pila) :a)) 10)) " " 
    (+ ((peek pila) :y) (* (Math/sin ((peek pila) :a)) -10)) " " 
)))

(defn iter_svg
  "Itera i veces"
  [i formula angulo pila salida]
  ( let [pila_mut pila] ( if (< i (count formula)) (
    ( iter_svg (+ i 1) formula angulo (procesar_pila angulo pila_mut (get formula i)) (procesar_salida pila_mut (get formula i)))
    salida ) ( str nil ) )
  ) 
)

;;(defn lineInLine_santi
;;    "Itera i veces"
;;    [ formula angulo elemento pila salida] (
;;    (if ( (not (empty? formula) ))
;;      (lineInLine_santi formula (next formula) angulo pila (+ i 1) (procesado ( get ( get formula i) 0 ) angulo pila salida) )
;;      salida
;;    ) 
;;  )
;;)

;;( defn LIL [formula angulo pila ]( str (
;;  map () formula
;;)))

(defn grados-a-radianes [angulo] 
  ( / (* angulo Math/PI ) 180)
)

(defn writeSvg [expresions angulo salida] 
  (spit salida "<svg viewBox=\"0 0 1000 1000\" xmlns=\"http://www.w3.org/2000/svg\">" :append true)
;;  (spit salida (str "<path d=\"M 10 250" (str (lineInLine_Testa expresions (grados-a-radianes angulo) (list {:x 10, :y 250, :a (grados-a-radianes 0)})) (char 34)) " stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>") :append true) 
  (spit salida (str "<path d=\"M 10 250" ( iter_svg 0 expresions (grados-a-radianes angulo) (list {:x 10, :y 250, :a (grados-a-radianes 0)}) (str nil) )))
  (spit salida "</svg>" :append true)
)