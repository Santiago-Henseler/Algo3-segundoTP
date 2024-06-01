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

(defn lineInLine_Testa 
  " Primer implementación con recursión directa. Pésimo manejo de memoria, no mas de cuatro iterac"
  [ formula angulo pila ] 
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
  if (= (get elemento 0) "[") (conj pila (peek pila)) (
  if (= (get elemento 0) "]") (pop pila) (
  if (= (get elemento 0) "+") (conj (pop pila) {
    :x ((peek pila) :x), 
    :y ((peek pila) :y), 
    :a (+ ((peek pila) :a) angulo) 
    } ) (
  if (= (get elemento 0) "-") (conj (pop pila) {
    :x ((peek pila) :x), 
    :y ((peek pila) :y), 
    :a (- ((peek pila) :a) angulo) 
    } ) ( conj (pop pila) { 
    :x (+ ((peek pila) :x) (* (Math/cos ((peek pila) :a)) 10)), 
    :y (+ ((peek pila) :y) (* (Math/sin ((peek pila) :a)) -10)), 
    :a (get (peek pila) :a)
    } 
))))))

(defn procesar_salida [pila elemento] (
  if (= (get elemento 0) "]") (
    str "M " 
      ((peek (pop pila)) :x) " " 
      ((peek (pop pila)) :y) " " 
  ) (
    str "L "
    (+ ((peek pila) :x) (* (Math/cos ((peek pila) :a)) 10)) " " 
    (+ ((peek pila) :y) (* (Math/sin ((peek pila) :a)) -10)) " " 
)))

(defn grados-a-radianes [angulo] 
  ( / (* angulo Math/PI ) 180)
)

(defn loop_svg 
  [formula angulo]
  ( 
    loop [i 0 pila (procesar_pila angulo (list {:x 10, :y 250, :a (grados-a-radianes 0)}) (get formula 0)) salida (str nil)]
    (
      if (< i (count formula)) 
        ( recur (+ i 1) (procesar_pila angulo pila (get formula i)) (str salida (procesar_salida pila (get formula i) )))
        salida
    )
  )
)

;;(defn iter_svg
;;  "Itera i veces"
;;  [i formula angulo pila salida]
;;  ( 
;;    let [tmp_salida (
;;      if (< i (count formula)) 
;;      ( iter_svg (+ i 1) formula angulo (procesar_pila angulo pila (get formula i)) (procesar_salida pila (get formula i)))
;;      ( str nil )
;;    )]
;;    str salida tmp_salida
;;  )
;;)

;;(defn lineInLine_Testa2 [ formula angulo pila ] ( if (empty? formula) (str nil)(
;;  str ( procesar_salida pila (first formula) ) " " ( lineInLine_Testa2 (next formula) angulo (procesar_pila angulo pila (first formula)))
;;)))

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

(defn writeSvg [expresions angulo salida] 
  (spit salida "<svg viewBox=\"0 0 1000 1000\" xmlns=\"http://www.w3.org/2000/svg\">")
;;  (spit salida (str "<path d=\"M 10 250" (str (lineInLine_Testa expresions (grados-a-radianes angulo) (list {:x 10, :y 250, :a (grados-a-radianes 0)})) (char 34)) " stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>") :append true) 
;;  (spit salida (str "<path d=\"M 10 250" (str (lineInLine_Testa2 expresions (grados-a-radianes angulo) (list {:x 10, :y 250, :a (grados-a-radianes 0)})) (char 34)) " stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>") :append true) 
;;  (spit salida (str "<path d=\"M 10 250 " ( iter_svg 0 expresions (grados-a-radianes angulo) (list {:x 10, :y 250, :a (grados-a-radianes 0)}) (str nil) ) (char 34) " stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>") :append true)
  (spit salida (str "<path d=\"M 10 250 " ( loop_svg expresions ( grados-a-radianes angulo ) ) (char 34) " stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>") :append true)
  (spit salida "</svg>" :append true)

)