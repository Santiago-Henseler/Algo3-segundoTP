(ns tp2.svgMaker  (:gen-class))
(require '[tp2.svgMaker :as svgMaker])

(defn procesar_pila [angulo pila elemento] (
  if (= (get elemento 0) (char 91)) (conj pila (peek pila)) (   ;; [ -> Agregar otra tortuga
  if (= (get elemento 0) (char 93)) (pop pila) (                ;; ] -> Quitar tortuga
  if (= (get elemento 0) (char 43)) (conj (pop pila) {          ;; + -> Agregar map con :a + angulo
    :x ((peek pila) :x), 
    :y ((peek pila) :y), 
    :a (+ ((peek pila) :a) angulo) 
    } ) (
  if (= (get elemento 0) (char 45)) (conj (pop pila) {          ;; - -> Agregar map con :a + angulo
    :x ((peek pila) :x), 
    :y ((peek pila) :y), 
    :a (- ((peek pila) :a) angulo) 
    } ) ( conj (pop pila) {                                     ;; Agregar a pila posición nueva  
    :x (+ ((peek pila) :x) (* (Math/cos ((peek pila) :a)) 10)), 
    :y (+ ((peek pila) :y) (* (Math/sin ((peek pila) :a)) -10)), 
    :a ((peek pila) :a)
    } 
))))))

(defn procesar_salida [pila elemento] (
  if (= (get elemento 0) (char 93))(
    str "M " 
      ((peek (pop pila)) :x) " " 
      ((peek (pop pila)) :y) " " 
  ) ( if (and (not= (get elemento 0) (char 43)) (not= (get elemento 0) (char 45)) (not= (get elemento 0) (char 91))) (
    str "L "
    (+ ((peek pila) :x) (* (Math/cos ((peek pila) :a)) 10)) " "
    (+ ((peek pila) :y) (* (Math/sin ((peek pila) :a)) -10)) " "                        
  ) ( str nil )
)))

(defn grados-a-radianes [angulo] 
  ( / (* angulo Math/PI ) 180)
)

(defn loop_svg 
  [formula angulo]
  ( 
    loop [formula_mut formula
          pila (list {:x 0, :y 0, :a 0})
          salida (str nil)]
    (
      if (not (empty? formula_mut)) 
        ( recur (next formula_mut)                                         
                (procesar_pila angulo pila (first formula_mut))
                (str salida (procesar_salida pila (first formula_mut)))
        )
        salida
    )
  )
)


(defn writeSvg [expresions angulo salida] 
  (let [anguloRadianes (grados-a-radianes angulo)
       directivasTotales (count expresions) 
       factorRedimension (/ directivasTotales 500)
       centro (double(* factorRedimension -250))
       tamanio (double(* factorRedimension 1000))
  ]
  (spit salida (str"<svg viewBox=\" "  centro " " centro " " tamanio " " tamanio "\" xmlns=\"http://www.w3.org/2000/svg\" preserveAspectRatio=\"xMidYMid meet\" width=\"100%\" height=\"100%\" style=\"overflow: visible;\" >") :append true)
  (spit salida (str "<path d=\"M 0 0 " (loop_svg expresions anguloRadianes) "\" stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>") :append true)
  (spit salida "</svg>" :append true) 
  )
)