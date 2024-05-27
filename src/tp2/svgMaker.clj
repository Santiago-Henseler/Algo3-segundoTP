(ns tp2.svgMaker  (:gen-class))

(defn turtle
  [expresions vector xTur angulo pos]
  (if-not (empty? expresions)
    (if (= (first expresions) "F")
      (turtle (rest expresions) (conj vector (str " l 10 0")) xTur angulo (+ pos 10))
      (if (= (first expresions) "+")
        (turtle (rest expresions) (conj vector (str " l "angulo" " angulo)) xTur angulo pos) 
        (if (= (first expresions) "-") 
          (turtle (rest expresions) (conj vector (str " l -"angulo" -" angulo)) xTur angulo pos) 
          (if (= (first expresions) "X")
            (turtle (rest expresions) (conj vector (str " l 10 0")) xTur angulo (+ pos 10))
            (if (= (first expresions) "[")
              (turtle (rest expresions) vector (conj (vec xTur) pos) angulo pos)   
              (turtle (rest expresions) (conj vector (str " M "(peek xTur)" " 0)) (vec (drop-last xTur)) angulo (peek xTur))
            )))))
  vector
  )
)


(defn lineInLine [expresions angulo] 
  ; (map (partial a angulo) expresions)
)
 
(defn writeSvg [expresions angulo] 
  (spit "test.svg" "<svg viewBox=\"-50 -150 2000 1000\" xmlns=\"http://www.w3.org/2000/svg\">" :append true)
  (spit "test.svg" (str "<path d=\"M 0 0"(apply str (turtle expresions [] [] (read-string angulo) 0))"\" stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>") :append true) 
  (spit "test.svg" "</svg>" :append true)
)