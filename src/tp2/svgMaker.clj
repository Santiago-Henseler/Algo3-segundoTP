(ns tp2.svgMaker  (:gen-class))
(require '[tp2.svgMaker :as svgMaker])

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

(defn procesarPila [angulo pila elemento] (
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
    :x (Math/round (+ ((peek pila) :x) (* (Math/cos ((peek pila) :a)) 10))), 
    :y (Math/round (+ ((peek pila) :y) (* (Math/sin ((peek pila) :a)) -10))), 
    :a ((peek pila) :a)
    } 
))))))

(defn procesarSalida [pila elemento] (
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
          pila (list {:x 10, :y (* 5 (count formula)), :a 0})
          salida (str nil)]
    (
      if (not (empty? formula_mut)) 
        ( recur (next formula_mut)                                         
                (procesarPila angulo pila (first formula_mut))
                (str salida (procesarSalida pila (first formula_mut)))
        )
        salida
    )
  )
)

(defn procesarFormulaSalida [pila elemento] (
  if (= (get elemento 0) (char 93))   ;; "]" desapilar tortuga 
    ( 
      vector
      ((peek (pop pila)) :x) 
      ((peek (pop pila)) :y) 
;;      (+ ((peek pila) :x) (* (Math/cos ((peek pila) :a)) 10))
;;      (+ ((peek pila) :y) (* (Math/sin ((peek pila) :a)) -10))
      0                        
    )
    ( if (and (not= (get elemento 0) (char 43)) (not= (get elemento 0) (char 45)) (not= (get elemento 0) (char 91))) 
    ;; No es "+" ni "-" ni "["
    (
      vector 
      (Math/round (+ ((peek pila) :x) (* (Math/cos ((peek pila) :a)) 10)))
      (Math/round (+ ((peek pila) :y) (* (Math/sin ((peek pila) :a)) -10)))
      ( if (Character/isLowerCase (get elemento 0)) 0 1)                        
    ) ;; Devuelve nil r case significa que es plumna levbanadn
    )
))

(defn procesarFormula 
  [formula angulo] (
    loop [formula_mut formula
          pila (list {:x 0, :y 0, :a 0})
          salida (list [0 0 0])]
    (
      if (empty? formula_mut)
        salida 
        ( recur ( next formula_mut)                                         
                ( procesarPila angulo pila (first formula_mut))
                ( if (nil? (procesarFormulaSalida pila (first formula_mut)) )
                  salida  ;; Si no dibuja otro punto, seguir con la misma salida 
                  (conj salida (procesarFormulaSalida pila (first formula_mut)))
                )
        )
    )
  )
)
(defn writeSvg [expresions angulo salida] 
  (spit salida (str "<svg viewBox=\"0 0 " (* 5 (count expresions)) " " (* 10 (count expresions)) (char 34) " xmlns=\"http://www.w3.org/2000/svg\">"))
;;  (println expresions)
;;  (spit salida (str "<path d=\"M 10 250" (str (lineInLine_Testa expresions (grados-a-radianes angulo) (list {:x 10, :y 250, :a (grados-a-radianes 0)})) (char 34)) " stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>") :append true) 
;;  (spit salida (str "<path d=\"M 10 250" (str (lineInLine_Testa2 expresions (grados-a-radianes angulo) (list {:x 10, :y 250, :a (grados-a-radianes 0)})) (char 34)) " stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>") :append true) 
;;  (spit salida (str "<path d=\"M 10 250 " ( iter_svg 0 expresions (grados-a-radianes angulo) (list {:x 10, :y 250, :a (grados-a-radianes 0)}) (str nil) ) (char 34) " stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>") :append true)
  (spit salida (str "<path d=\"M 10 0" ( loop_svg expresions ( grados-a-radianes angulo ) ) (char 34) " stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>") :append true)
  (spit salida "</svg>" :append true)
)

(defn encontrarMinimoX [expresiones](
  loop [minimo 0 expresiones_mut expresiones] (
    if (empty? expresiones_mut ) minimo 
    ( recur (min minimo (first (first expresiones_mut))) (next expresiones_mut) )
  )
))

(defn encontrarMinimoY [expresiones](
  loop [minimo 0 expresiones_mut expresiones] (
    if (empty? expresiones_mut ) minimo 
    ( recur (min minimo (second (first expresiones_mut))) (next expresiones_mut) )
  )
))

(defn encontrarMaximoX [expresiones](
  loop [maximo 0 expresiones_mut expresiones] (
    if (empty? expresiones_mut ) maximo 
    ( recur (max maximo (first (first expresiones_mut))) (next expresiones_mut) )
  )
))

(defn encontrarMaximoY [expresiones](
  loop [maximo 0 expresiones_mut expresiones] (
    if (empty? expresiones_mut ) maximo 
    ( recur (max maximo (second (first expresiones_mut))) (next expresiones_mut) )
  )
))

(defn encontrarMinimo [arg1 arg2](
  if ( > arg1 arg2 ) arg1 arg2 
))

(defn encontrarMaximo [arg1 arg2](
  if ( > arg1 arg2 ) arg1 arg2 
))

(defn procesarLinea 
  [punto](
  str (if (= (get punto 2) 0) (str "M ") (str "L ")) (get punto 0) " " (get punto 1) " "
))

(defn procesarLinea2 
  [ minimoX minimoY punto]
  (str (if (= (get punto 2) 0) (str "M ") (str "L ")) (- (get punto 0) minimoX) " " (- (get punto 1) minimoY) " ")
)

(defn !escribirSVG 
  ;; Pre: Lista de vectores de puntos del fractal y archivo de salida valido
  ;; Post: Escribe archivo de salida 
  [ expresiones salida ]

;;  (println (map first  expresiones))
  (spit salida (str "<svg viewBox=\""
    (-(encontrarMinimoX expresiones)10) " "
    (-(encontrarMinimoY expresiones)10) " "
    (+(- (encontrarMaximoX expresiones) (encontrarMinimoX expresiones))10) " "
    (+(- (encontrarMaximoY expresiones) (encontrarMinimoY expresiones)) 10) " "

;;    (-(reduce min (map first  expresiones))10) " "
;;    (-(reduce min (map second expresiones))10) " "
;;    (+(reduce max (map first  expresiones))10) " "
;;    (+(reduce max (map second expresiones))10) " "
    "\" xmlns=\"http://www.w3.org/2000/svg\" 
    preserveAspectRatio=\"xMidYMid meet\" width=\"100%\" height=\"100%\" style=\"overflow: visible;\" >"))
;;  (println (map procesarLinea expresiones) )
;;  (println expresiones )
  (spit salida (str "<path d=\""  (apply str ( map procesarLinea expresiones )) "\""
    " stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>" ) :append true)
  (spit salida "</svg>" :append true)
)

(defn !escribirSVG2 
  ;; Pre: Lista de vectores de puntos del fractal y archivo de salida valido
  ;; Post: Escribe archivo de salida 
  [ expresiones salida ]
 (println expresiones)
;;  (println salida)

  (spit salida (str "<svg viewBox=\" 0 0 "
    (- (reduce encontrarMaximo (map first  expresiones)) (reduce encontrarMinimo (map first  expresiones))) " "
    (- (reduce encontrarMaximo (map second expresiones)) (reduce encontrarMinimo (map second expresiones))) " "
    "\" xmlns=\"http://www.w3.org/2000/svg\">"))
  (println (map procesarLinea expresiones) )
  (spit salida (str "<path d=\"" (apply str ( map 
    (partial procesarLinea2 (reduce encontrarMinimo (map first  expresiones)) (reduce encontrarMinimo (map second  expresiones))) 
      expresiones ))
    "\" stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>" ) :append true )

  (spit salida "</svg>" :append true)
)