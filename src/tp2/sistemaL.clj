(ns tp2.sistemaL (:gen-class))
(use 'clojure.java.io)
(require '[clojure.string :as str])
(require '[tp2.svgMaker :as svgMaker])

(defn make-vec
  "Expande el vector con las reglas"
  [reglas vector]
  (map #(if (contains? reglas %) (list (str/split(reglas %)#"")) (list %)) vector)
)

(defn iter
  "Itera i veces"
  [reglas i vector]
  (if (> i 0)
    (iter reglas (- i 1) (flatten(make-vec reglas vector)))
    vector
    ) 
)

(defn reglas-dicc
  "Transforma las reglas en un diccionario"
  [reglas]
  (let [
       reglasVec (for [x (range 0 (count reglas))]
                    (conj (str/split (reglas x) #" ")))
       ]
    (apply hash-map (flatten reglasVec))
  ) 
)

;; Generosidad de https://stackoverflow.com/questions/5621279/in-clojure-how-can-i-convert-a-string-to-a-number
(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(defn openFile
  [file i salida]
  (let [
        archivo (with-open [rdr (reader (str "doc/" file))] 
                  (reduce conj [] (line-seq rdr)) ;; Crear lista de lineas
                  )
        angulo (archivo 0)
        axioma (archivo 1) 
        reglas (subvec archivo 2);; Vector con las lineas.
        ]
    (
      do
      (println axioma)
      (svgMaker/writeSvg (iter (reglas-dicc reglas) i (list (str/split axioma #""))) (parse-int angulo) salida)
    )
    )
  )