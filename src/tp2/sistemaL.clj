(ns tp2.sistemaL (:gen-class))
(use 'clojure.java.io)
(require '[clojure.string :as str])
(require '[tp2.svgMaker :as svgMaker])

(defn make-vec
  [reglas vector]
  (map #(if (contains? reglas %) (list (str/split(reglas %)#"")) (list %)) (vec vector))
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

(defn openFile
  [file i]
  (let [
        archivo (with-open [rdr (reader (str "doc/" file))] 
                  (reduce conj [] (line-seq rdr)) ;; Crear lista de lineas
                  )
        angulo (archivo 0)
        axioma (archivo 1) 
        reglas (subvec archivo 2);; Vector con las lineas.
        ]
    (print (iter (reglas-dicc reglas) i (list axioma)))
    )
  )