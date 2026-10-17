(ns distil
  (:import java.io.File))

(let [f (File. ".")]
  (println (.getAbsolutePath f)))

(.. (File. ".") getAbsolutePath getBytes)
