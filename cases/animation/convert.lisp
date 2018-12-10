(cl:in-package :cl-bodge.animation.demo)


(defun build-model ()
  (bodge-converter:with-new-resource-file
      (stream (merge-showcase-pathname "animation/assets/cyber_warrior.brf") :if-exists :supersede)
    (bodge-converter:write-scene stream (merge-showcase-pathname
                                         "animation/assets/source/cyber_warrior.fbx")
                                 :scene-name "cyber-warrior"
                                 :embed-textures nil)))
