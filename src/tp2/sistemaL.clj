(ns tp2.sistemaL (:gen-class))
(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn make-vec
  "Reemplaza el vector por las reglas"
  [reglas vector]
    (println vector)
  (let [
       nilVec '() 
       newVec (for [x (range 0 (count vector))] 
        (if (contains? reglas ((vec vector) x))
        (flatten(first (conj nilVec (apply list (str/split(reglas ((vec vector) x))#"")))))
        (conj nilVec ((vec vector) x))
       ))]
    newVec
  )
)

(defn iter
  "Itera i veces"
  [reglas i vector]
  (if (> i 1)
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
    (println (iter (reglas-dicc reglas) i (list axioma)))
    )
  )
