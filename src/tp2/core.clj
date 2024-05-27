(ns tp2.core (:gen-class))
(require '[tp2.sistemaL :as sistemaL])
(require '[tp2.svgMaker :as svgMaker])


(defn -main [& args]
  ( if (= (count args) 3)
   (let [
        ;; Parseo de argumentos
        argumentos (vec args)
        entrada (argumentos 0)
        iteraciones ( Integer/parseInt (argumentos 1))
        salida (argumentos 2) 
   ]
      ;; Parseo de archivo   
     (sistemaL/openFile entrada iteraciones salida)
   )
   
  ( println "Uso: lein run [archivo_entrada.sl] [cant_iteraciones] [archivo_salida.svg]") )
)
