(cl:defpackage :cl-bodge.test.demo
  (:use :cl :bodge-demo.api))
(cl:in-package :cl-bodge.test.demo)

;;;
;;; Test showcase
;;;
(defclass test-showcase () ())


(ge:defamalgam test-amalgam
  (color :vec4))


(ge:defshader (test-shader
               (:sources "test.glsl")
               (:base-path (merge-showcase-pathname "test/")))
  (color :name "color"))


(ge:defpipeline (test-pipeline
                 (:primitive :points))
  :vertex test-shader
  :fragment test-shader)


(defvar *pipeline* nil)


(defclass test-showcase () ())


(register-showcase 'test-showcase "Test")


(defmethod showcase-revealing-flow ((this test-showcase) ui)
  (ge:for-graphics ()
    (setf *pipeline* (ge:make-shader-pipeline 'test-pipeline))))


(defmethod showcase-closing-flow ((this test-showcase))
  (ge:instantly ()
    (ge:dispose *pipeline*)))


(defmethod render-showcase ((this test-showcase))
  (ge:clear-rendering-output t :color (ge:vec4 0.2 0.2 0.2 1.0))
  (ge:render t *pipeline*
             :vertex-count 1
             'color (ge:vec4 0.8 0.8 0.8 1.0)))
