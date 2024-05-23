(ns tp2.svgMaker  (:gen-class))

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
 
(defn writeSvg [expresions angulo] 
  (spit "test.svg" "<svg viewBox=\"-50 -150 300 200\" xmlns=\"http://www.w3.org/2000/svg\">" :append true)
  (spit "test.svg" (str "<path d=\"M 0 0" (apply str (lineInLine expresions angulo))  " stroke-width=\"1\" stroke=\"black\" fill=\"none\"/>") :append true) 
  (spit "test.svg" "</svg>" :append true)
)