(ns tp2.core (:gen-class))
(require '[tp2.sistemaL :as sistemaL])
(require '[tp2.svgMaker :as svgMaker])
(require '[clojure.string :as str])


(defn -main [& args] ( 
  if (= (count args) 3) 
  (  let [ 
      ;; Parseo de argumentos
      argumentos (vec args)
      entrada (argumentos 0)
      iteraciones ( Integer/parseInt (argumentos 1))
      salida (argumentos 2) 

      ;; Parseo de archivo 
      lineas (sistemaL/!abrirArchivo entrada)
      angulo (Integer/parseInt (lineas 0))
      axioma (lineas 1)
      reglas (lineas 2)      
    ] 
;;    (sistemaL/!openFile entrada iteraciones salida) ;; Parseo de archivo   
    (svgMaker/!escribirSVG (
      svgMaker/procesarFormula (
        sistemaL/iter (sistemaL/reglas-dicc reglas) iteraciones (list (str/split axioma #""))) angulo) 
    salida)

  )
  ( println "Uso: lein run [archivo_entrada.sl] [cant_iteraciones] [archivo_salida.svg]") 
))
