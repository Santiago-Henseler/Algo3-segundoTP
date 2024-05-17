(ns tp2.sistemaL (:gen-class))
(use 'clojure.java.io)

(defn openFile 
    []
    (with-open [rdr (reader "/d/arbol1.sl")]
        (doseq [line (line-seq rdr)]
            (println line)))    
)
