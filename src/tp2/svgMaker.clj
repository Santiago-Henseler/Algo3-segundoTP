(ns tp2.svgMaker  (:gen-class))
(require '[tp2.svgMaker :as svgMaker])

(defn grados-a-radianes [angulo] (/ (* angulo Math/PI) 180))

(defn procesarPila 
  "Pre: Copia de pila, angulo y elemento de formula
  Post: Copia de pila actualizada segun elemento"
  [angulo pila elemento] (
  if (= (get elemento 0) (char 91)) (conj pila (peek pila)) (   ;; [ -> Agregar otra tortuga
  if (= (get elemento 0) (char 93)) (pop pila) (                ;; ] -> Quitar tortuga
  if (= (get elemento 0) (char 43)) (conj (pop pila) {          ;; + -> Girar tortuga en sentido anti horario
    :x ((peek pila) :x), 
    :y ((peek pila) :y), 
    :a (+ ((peek pila) :a) angulo) 
    } ) (
  if (= (get elemento 0) (char 45)) (conj (pop pila) {          ;; - -> Girar tortuga en sentido horario
    :x ((peek pila) :x), 
    :y ((peek pila) :y), 
    :a (- ((peek pila) :a) angulo) 
    } ) (
  if (= (get elemento 0) (char 124)) (conj (pop pila) {         ;; | -> Dar vuelta la tortuga ( girar 180º )
      :x (Math/round (+ ((peek pila) :x) (* (Math/cos ((peek pila) :a)) 10))),
      :y (Math/round (+ ((peek pila) :y) (* (Math/sin ((peek pila) :a)) -10))),
      :a (+((peek pila) :a) (grados-a-radianes 180))}) 
     (conj (pop pila) {                                         ;; Agregar posición nueva en la pila  
      :x (Math/round (+ ((peek pila) :x) (* (Math/cos ((peek pila) :a)) 10))),
      :y (Math/round (+ ((peek pila) :y) (* (Math/sin ((peek pila) :a)) -10))),
      :a ((peek pila) :a)})
))))))

(defn procesarFormulaSalida 
  "Pre: Pila y elemento de la formula
  Post: Vector X-Y-Indicador pluma arriba/abajo. Nil si no hay punto nuevo"
  [pila elemento] (
  if (= (get elemento 0) (char 93))   ;; "]" desapilar tortuga 
    ( vector 
      ((peek (pop pila)) :x) 
      ((peek (pop pila)) :y) 
      0 
    ) ( if (and (not= (get elemento 0) (char 43)) (not= (get elemento 0) (char 45)) (not= (get elemento 0) (char 91))) 
    ;; No es "+" ni "-" ni "[" -> Actualizar posicion
    ( vector 
      (Math/round (+ ((peek pila) :x) (* (Math/cos ((peek pila) :a)) 10)))
      (Math/round (+ ((peek pila) :y) (* (Math/sin ((peek pila) :a)) -10)))
      ( if (Character/isLowerCase (get elemento 0)) 0 1)                        
    ) ;; Devuelve nil para casos que no generan nuevos puntos ( + - [ )
)))

(defn procesarFormula 
  "Pre: Formula y angulo validos
  Post: Lista de puntos con indicador de pluma arriba o abajo"
  [formula angulo] (
    loop [formula_mut formula
          pila (list {:x 0, :y 0, :a 0})
          salida (list [0 0 0])]
    ( if (empty? formula_mut) salida 
      ( recur ( next formula_mut)                                         
              ( procesarPila angulo pila (first formula_mut)) 
              ;; Si no dibuja otro punto (ej: tortuga desapilada/rotación de tortuga), seguir con la misma salida 
              ( if (nil? (procesarFormulaSalida pila (first formula_mut)) ) salida (conj salida (procesarFormulaSalida pila (first formula_mut))) )
      )
)))

(defn encontrarMinimoX [expresiones](
  loop [minimo 0 expresiones_mut expresiones] ( 
    if (empty? expresiones_mut ) minimo ( recur (min minimo (first (first expresiones_mut))) (next expresiones_mut) )
)))

(defn encontrarMinimoY [expresiones](
  loop [minimo 0 expresiones_mut expresiones] (
    if (empty? expresiones_mut ) minimo ( recur (min minimo (second (first expresiones_mut))) (next expresiones_mut) )
)))

(defn encontrarMaximoX [expresiones](
  loop [maximo 0 expresiones_mut expresiones] (
    if (empty? expresiones_mut ) maximo ( recur (max maximo (first (first expresiones_mut))) (next expresiones_mut) )
)))

(defn encontrarMaximoY [expresiones](
  loop [maximo 0 expresiones_mut expresiones] (
    if (empty? expresiones_mut ) maximo ( recur (max maximo (second (first expresiones_mut))) (next expresiones_mut) )
)))

(defn procesarLinea 
  "Pre: Punto válido
  Post: Texto en formato SVG"
  [punto]
  ( str (if (= (get punto 2) 0) (str "M ") (str "L ")) (get punto 0) " " (get punto 1) " ")
)

(defn !escribirSVG 
  "Pre: Lista de vectores de puntos del fractal y archivo de salida valido
   Post: Escribe archivo de salida "
  [ expresiones salida ]
  (spit salida (str "<svg viewBox=\""
    (-(encontrarMinimoX expresiones)10) " "
    (-(encontrarMinimoY expresiones)10) " "
    (+(- (encontrarMaximoX expresiones) (encontrarMinimoX expresiones))10) " "
    (+(- (encontrarMaximoY expresiones) (encontrarMinimoY expresiones)) 10) " "
    "\" xmlns=\"http://www.w3.org/2000/svg\" preserveAspectRatio=\"xMidYMid meet\" width=\"100%\" height=\"100%\" style=\"overflow: visible;\" >"))
  (spit salida (str "<path d=\""  (apply str ( map procesarLinea expresiones )) "\" stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>" ) :append true)
  (spit salida "</svg>" :append true)
)