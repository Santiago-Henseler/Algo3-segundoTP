(ns tp2.sistemaL (:gen-class))
(use 'clojure.java.io)

(defn openFile 
    [x]
    (with-open [rdr (reader (str "doc/" x))]
        (reduce conj [] (line-seq rdr)) ;; Crear lista de lineas
    )    
)
