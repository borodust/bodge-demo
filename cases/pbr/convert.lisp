(in-package :cl-bodge.pbr.demo)


(defun build-model ()
  (bodge-converter:with-new-resource-file
      (stream (merge-showcase-pathname "pbr/assets/DamagedHelmet.brf") :if-exists :supersede)
    (bodge-converter:write-scene stream (merge-showcase-pathname "pbr/assets/DamagedHelmet.gltf")
                                 :scene-name "DamagedHelmet")))
