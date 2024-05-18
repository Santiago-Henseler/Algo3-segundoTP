(ns tp2.core (:gen-class))
(require '[tp2.sistemaL :as sistemaL])
(require '[clojure.string :as str])


(defn -main [& args]
  ( if (= (count args) 3) ( do
    ;; Parseo de argumentos
    (def argumentos (vec args))
    (def entrada (argumentos 0))
    (def iteraciones (argumentos 1))
    (def salida (argumentos 2))

    ;; Parseo de archivo 
    (def archivo ( sistemaL/openFile entrada ) )
    (def angulo (archivo 0))
    (def axioma (archivo 1))
    (def reglas (subvec archivo 2)) ;; Vector con las lineas.
    (println (str/split (reglas 0) #" ")) ;;  Separando por espacio 
  )
  ( println "Uso: lein run [archivo_entrada.sl] [cant_iteraciones] [archivo_salida.svg]") )
)
