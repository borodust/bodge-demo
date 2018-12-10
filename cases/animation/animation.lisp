(cl:defpackage :cl-bodge.animation.demo
  (:use :cl :bodge-demo.api))
(cl:in-package :cl-bodge.animation.demo)

;;;
;;; Cyber warrior https://sketchfab.com/models/86f58bf5151c410facacf0ed6a2ebd53
;;; by Orbelin_Jaimes
;;;

;;;
;;; Animation showcase
;;;
(defclass animation-showcase () ())


(defmethod initialize-instance :after ((this animation-showcase) &key)
  (ge:mount-container "/bodge/demo/animation/scene/"
                      (merge-showcase-pathname "animation/assets/cyber_warrior.brf"))
  (ge:mount-filesystem "/bodge/demo/animation/assets/"
                       (merge-showcase-pathname "animation/assets/")))


(defmethod showcase-revealing-flow ((this animation-showcase) ui))


(defmethod showcase-closing-flow ((this animation-showcase)))


(defmethod render-showcase ((this animation-showcase))
  (ge:clear-rendering-output t :color (ge:vec4 0.2 0.2 0.2 1.0)))
