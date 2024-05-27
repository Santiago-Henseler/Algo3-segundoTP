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
      if (= (first formula) "[") (    ;; Esto es un gran if else if else if else ...
        ;; Agregamos a la pila el mismo elemento que tenemos antes
;;        ( println "DEBUG 2 LILT")
        ( lineInLine_Testa (next formula) angulo (conj pila (peek pila)) )  
      ) ( 
      if (= (first formula) "]") (
;;        ( println "DEBUG 3 LILT")
        ( str "M " (nth (peek pila) 0) " " (nth (peek pila) 1) " " (lineInLine_Testa  (next formula) angulo (pop pila) ) ) 
        ;; Quitar tortuga y escribir linea con pluma levantada para volver a punto inicial 
      ) (
      if (= (first formula) "+") (
;;        ( println "DEBUG 4 LILT")
        ( lineInLine_Testa (next formula) angulo (conj (pop pila) '(vector '(nth (peek pila) 0) '(nth (peek pila) 1) '(+ (nth (peek pila) 2) angulo))) ) 
      ) (
      if (= (first formula) "-") (
;;        ( println "DEBUG 5 LILT")        
        ( lineInLine_Testa (next formula) angulo (conj (pop pila) '(vector '(nth (peek pila) 0) '(nth (peek pila) 1) '(- (nth (peek pila) 2) angulo))) )
      ) (       ;; Else
;;        ( println "DEBUG 6 LILT")    
        ( str "L " (nth ( vector '(+ (nth (peek pila) 0) (* (math/cos angulo) 100)) '(+ (nth (peek pila) 1) (* (* (math/sin angulo) 100) -1)) ) 0) " " (nth ( vector '(+ (nth (peek pila) 0) (* (math/cos angulo) 100)) '(+ (nth (peek pila) 1) (* (* (math/sin angulo) 100) -1)) ) 1) " " (lineInLine_Testa  (next formula) angulo pila ) )   
      )))) 
    )
    (
;;      ( println "DEBUG 7 LILT")    
        ( str "" )
;;      ( str nil )
;;      ( println "DEBUG 7 LILT OUT")    
    )
  )
)

(defn writeSvg [expresions angulo salida] 
  (println "DEBUG 0 writesvg")
  (spit salida "<svg viewBox=\"-50 -150 300 200\" xmlns=\"http://www.w3.org/2000/svg\">" :append true)
  (println "DEBUG 1 writesvg")
  (if (string? ( lineInLine_Testa expresions angulo (list '(vector '0 '0 'angulo)) ) ) (println "Es string") (println "No es string"))
  (println "DEBUG 2 writesvg")
;;  (spit salida (str "<path d=\"M 0 0" (str (lineInLine_Testa expresions angulo (list '(vector '0 '0 'angulo))) (char 34)) " stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>") :append true) 
  (println "DEBUG 3 writesvg")
  (spit salida "</svg>" :append true)
)