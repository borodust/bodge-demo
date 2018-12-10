(cl:defpackage :cl-bodge.text.demo
  (:use :cl :bodge-demo.api))
(cl:in-package :cl-bodge.text.demo)


(defclass text-showcase ()
  (font text-renderer pipeline text
   (proj :initform (ge:perspective-projection-mat 200 150 1 100))))


(defmethod initialize-instance :after ((this text-showcase) &key)
  (ge:mount-container "/bodge/demo/text/" (merge-showcase-pathname "text/assets/NotoSans-Regular.brf")))


(register-showcase 'text-showcase "SDF Text")


(defmethod showcase-revealing-flow ((this text-showcase) ui)
  (with-slots (font text-renderer text pipeline) this
    (ge.ng:>>
     (ge:for-graphics ()
       (setf font (ge:build-sdf-font "/bodge/demo/text/NotoSans-Regular")
             text (ge:make-text "WEEEEEEEEEEE!" font)
             pipeline (ge:make-shader-pipeline 'ge:text-pipeline)
             text-renderer (ge:make-text-renderer 800 600 font 64))))))


(defmethod showcase-closing-flow ((this text-showcase))
  (with-slots (font text-renderer text pipeline) this
    (ge:instantly ()
      (ge:dispose pipeline)
      (ge:dispose text)
      (ge:dispose text-renderer)
      (ge:dispose font))))


(defmethod render-showcase ((this text-showcase))
  (with-slots (text text-renderer pipeline font proj) this
    (let ((z-distance (- (+ 41 (* 40 (sin (bodge-util:real-time-seconds)))))))
      (ge:render-text t pipeline text
                      :scale 3f0
                      :base-color (ge:vec4 0.5 0.75 1 1)
                      :mvp-matrix (ge:mult proj (ge:translation-mat4-homo -200 0 z-distance))))
    (ge:print-text text-renderer "Uninspiring text" :position (ge:vec2 310 500))))
