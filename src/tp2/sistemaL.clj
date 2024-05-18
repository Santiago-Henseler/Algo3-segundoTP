(ns tp2.sistemaL (:gen-class))
(use 'clojure.java.io)

(defn openFile 
    [x]
    (with-open [rdr (reader (str "doc/" x))]
        (doseq [line (line-seq rdr)]
            (println line)))    
)
