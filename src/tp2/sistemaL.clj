(ns tp2.sistemaL (:gen-class))
(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn make-vec
  "Expande el vector con las reglas"
  [reglas vector]
  (map #(if (contains? reglas %) (list (str/split(reglas %)#"")) (list %)) vector)
)

(defn filtrado
  "Elimina los elementos que no pertenecen a las reglas"
  [reglas vector]
;;  (println vector)
  (filter #(contains? reglas %) vector)
)

(defn iter
  "Itera i veces"
  [reglas i vector]
  (if (> i -1)
    (iter reglas (- i 1) (flatten(make-vec reglas vector)))
    (filtrado reglas vector)
    ) 
)

(defn reglas-dicc
  "Transforma las reglas en un diccionario"
  [reglas]
  (let [
       reglasVec (for [x (range 0 (count reglas))]
                    (conj (str/split (reglas x) #" ")))
       ]
    (merge (apply hash-map (flatten reglasVec)) {"-" "-", "+" "+", "]" "]", "[" "["} )
  ) 
)

(defn !abrirArchivo 
  ;; Pre: Archivo valido
  ;; Post: [angulo axioma [reglas]]
  [archivo] (
    let [ lineas (with-open [rdr (reader (str "doc/" archivo))] 
                    (reduce conj [] (line-seq rdr))) ;; Crear lista de lineas
    ] (vector (lineas 0) (lineas 1) (subvec lineas 2))
))