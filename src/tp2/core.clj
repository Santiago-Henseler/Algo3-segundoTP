(ns tp2.core (:gen-class))
(require '[tp2.sistemaL :as sistemaL])


(defn -main [& args]
  ( if (= (count args) 3) ( do
    (def argumentos (vec args))
    (def entrada (argumentos 0))
    (def iteraciones (argumentos 1))
    (def salida (argumentos 2))
    ( sistemaL/openFile entrada )
  )
  ( println "Uso: lein run [archivo_entrada.sl] [cant_iteraciones] [archivo_salida.svg]") )
)
