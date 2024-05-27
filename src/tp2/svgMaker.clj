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

(defn lineInLine_Testa [ formula angulo pila ] ;; pila inicialmente será (0 0 angulo)
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
          (+ ((peek pila) :x) (* (Math/cos ((peek pila) :a)) 100)) " " 
          (+ ((peek pila) :y) (* (Math/sin ((peek pila) :a)) -100)) " " 
          ( lineInLine_Testa  (next formula) angulo (conj (pop pila) { 
            :x (+ ((peek pila) :x) (* (Math/cos ((peek pila) :a)) 100)), 
            :y (+ ((peek pila) :y) (* (Math/sin ((peek pila) :a)) -100)), 
            :a (get (peek pila) :a)
            })
          ) " "  
        
      ))))
    ) 
    ( str nil ) 
  )
)

(defn grados-a-radianes [angulo] 
  ( / (* angulo Math/PI ) 180)
)

(defn writeSvg [expresions angulo salida] 
  (println "DEBUG 0 writesvg")
  (spit salida "<svg viewBox=\"-50 -150 300 200\" xmlns=\"http://www.w3.org/2000/svg\">" :append true)
  (println "DEBUG 1 writesvg")
;;  (lineInLine_Testa expresions angulo ('(hash-map :x 0, :y 0, :a 0)) )
  (spit salida (str "<path d=\"M 0 0" (str (lineInLine_Testa expresions angulo (list {:x 0, :y 0, :a 0})) (char 34)) " stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>") :append true) 
  (println "DEBUG 2 writesvg")
;; (spit salida (str "<path d=\"M 0 0" (str (lineInLine_Testa expresions angulo (list '(vector '0 '0 'angulo))) (char 34)) " stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>") :append true) 
  (println "DEBUG 3 writesvg")
  (spit salida "</svg>" :append true)
)