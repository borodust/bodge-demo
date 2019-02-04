(cl:defpackage :bodge-demo.scene
  (:use :cl)
  (:export #:make-simple-scene
           #:render-scene
           #:update-view
           #:update-light
           #:update-drawable
           #:add-sphere
           #:add-box
           #:add-mesh))
(cl:in-package :bodge-demo.scene)


(declaim (special *depth-mvp*))

(defparameter *near-far* (ge:vec2 0.01 10.01))

(ge:defshader (demo-shader
               (:sources "demo.glsl")
               (:base-path :system-relative :bodge-demo "shaders/"))
  (position :name "vPosition")
  (normal :name "vNormal")
  (color :name "diffuseColor")
  (emission-color :name "emissionColor")
  (model :name "model")
  (near-far-vec :name "nearFar")
  (view :name "view")
  (projection :name "projection")
  (material :name "material")
  (light :name "light")
  (shadow-map :name "shadowMap"))


(ge:defpipeline (demo-pipeline
                 (:primitive :triangle-strip))
  :vertex demo-shader
  :fragment demo-shader)


(defclass scene (ge:disposable)
  ((pipeline :reader %pipeline-of)
   (depth-pipeline :reader %depth-pipeline-of)
   (light :initform (ge:make-shader-structure 'ge:phong-point-light
                                              :position (ge:vec3 0 0 0)
                                              :color (ge:vec3 1 1 1)
                                              :ambient (ge:vec3 0.3 0.3 0.3)
                                              :falloff 0.15f0
                                              :radius 100f0)
          :reader %light-of)
   (proj :initarg :projection-matrix
         :accessor %proj-of)
   (view :initform (ge:identity-mat4)
         :accessor %view-of)
   (shapes :initform (list nil))
   (shadow-map :initform nil :reader %shadow-map-of)))


(ge:define-destructor scene (pipeline shapes depth-pipeline)
  (loop for shape in shapes
        when shape do (ge:dispose shape))
  (ge:dispose depth-pipeline)
  (ge:dispose pipeline))


(defmethod initialize-instance :after ((this scene) &key)
  (with-slots (pipeline depth-pipeline shadow-map) this
    (setf pipeline (ge:make-shader-pipeline 'demo-pipeline)
          depth-pipeline (ge:make-shader-pipeline 'ge:depth-pipeline)
          shadow-map (ge:make-empty-depth-cubemap-texture 1024))))


(defun update-view (scene &key (position (ge:vec3 0 0 0)) (rotation (ge:vec3 0 0 0)))
  (with-slots (view) scene
    (setf view (ge:mult (ge:vec->translation-mat4-homo position)
                        (ge:euler-angles->mat4-homo rotation)))))


(defun update-light (scene &key color position)
  (with-slots (light) scene
    (when color
      (setf (ge:phong-point-light-color light) color))
    (when position
      (setf (ge:phong-point-light-position light) position))))


(defun make-simple-scene (&key projection-matrix)
  (make-instance 'scene
                 :projection-matrix (or projection-matrix
                                        (ge:perspective-projection-mat 1 (/ 480 640) 1.0 100.0))))


(defclass shape (ge:disposable)
  ((position-buffer)
   (normal-buffer)
   (index-buffer)
   (transform :initform (ge:identity-mat4))
   (color :initform (ge:vec3 1 1 1))
   (primitive :initform :triangle-strip :initarg :primitive)
   (emission-color :initform (ge:vec3 0 0 0))
   (material :initform (ge:make-shader-structure 'ge:phong-material
                                                 :specular-scale 0.65f0
                                                 :shininess 25f0
                                                 :roughness 10f0
                                                 :albedo 0.95f0))))


(defmethod initialize-instance :after ((this shape) &key vertex-generator)
  (with-slots (position-buffer normal-buffer index-buffer) this
    (multiple-value-bind (vertices normals indices)
        (funcall vertex-generator)
      (setf position-buffer (ge:make-array-buffer vertices :element-size 3)
            normal-buffer (ge:make-array-buffer normals :element-size 3)
            index-buffer (ge:make-index-buffer indices)))))


(ge:define-destructor shape (position-buffer normal-buffer index-buffer)
  (ge:dispose position-buffer)
  (ge:dispose normal-buffer)
  (ge:dispose index-buffer))


(defun render-shape (output scene shape)
  (with-slots (position-buffer normal-buffer index-buffer material
               transform color emission-color primitive)
      shape
    (ge:render output (%pipeline-of scene)
               :index-buffer index-buffer
               :primitive primitive
               'position position-buffer
               'normal normal-buffer
               'material material
               'light (%light-of scene)
               'model transform
               'view (%view-of scene)
               'projection (%proj-of scene)
               'color color
               'near-far-vec *near-far*
               'emission-color emission-color
               'shadow-map (%shadow-map-of scene))))


(defun render-shape-depth (output scene shape)
  (with-slots (position-buffer index-buffer transform primitive) shape
    (ge:render-with-depth-pipeline output (%depth-pipeline-of scene)
                                   (ge:mult *depth-mvp* transform)
                                   position-buffer
                                   :index-buffer index-buffer
                                   :primitive primitive)))


(defun update-drawable (shape &key color transform material emission-color)
  (with-slots ((this-color color)
               (this-transform transform)
               (this-material material)
               (this-emission-color emission-color))
      shape
    (when color
      (setf this-color color))
    (when transform
      (setf this-transform transform))
    (when material
      (setf this-material material))
    (when emission-color
      (setf this-emission-color emission-color))))


(defclass sphere (shape) ())

(defun add-sphere (scene &key (radius 1.0))
  (with-slots (shapes) scene
    (flet ((vertex-gen ()
             (bodge-demo::generate-sphere-arrays radius 100 100)))
      (let ((sphere (make-instance 'sphere :vertex-generator #'vertex-gen)))
        (push sphere shapes)
        sphere))))


(defclass box (shape) ()
  (:default-initargs :primitive :triangles))


(defun add-box (scene &key (x 1) (y 1) (z 1))
  (with-slots (shapes) scene
    (flet ((vertex-gen ()
             (bodge-demo::generate-box-arrays x y z)))
      (let ((box (make-instance 'box :vertex-generator #'vertex-gen)))
        (push box shapes)
        box))))


(defclass mesh (shape) ())


(defun add-mesh (scene position-array index-array normal-array primitive)
  (with-slots (shapes) scene
    (flet ((vertex-gen ()
             (values position-array normal-array index-array)))
      (let ((mesh (make-instance 'mesh :vertex-generator #'vertex-gen :primitive primitive)))
        (push mesh shapes)
        mesh))))


(defun %render-scene (scene rendering-fu &optional (output t))
  (with-slots (pipeline shapes) scene
    (loop for shape in shapes
          when shape
            do (funcall rendering-fu output scene shape))))


(defun render-to-shadow-map (scene shadow-layer rotation)
  (with-slots (light shadow-map) scene
    (let* ((light-position (ge:mult -1 (ge:phong-point-light-position light)))
           (*depth-mvp* (ge:mult (ge:perspective-projection-mat (* 2 (ge:x *near-far*))
                                                                (* 2 (ge:x *near-far*))
                                                                (ge:x *near-far*)
                                                                (ge:y *near-far*))
                                 rotation
                                 (ge:translation-mat4-homo (ge:x light-position)
                                                           (ge:y light-position)
                                                           (ge:z light-position)))))
      (%render-scene scene #'render-shape-depth shadow-layer))))


(defun render-shadow-map (scene)
  (with-slots (light shadow-map) scene
    (ge:clear-rendering-output shadow-map)
    (ge:do-cubemap-layers ((layer rotation) shadow-map)
      (render-to-shadow-map scene layer rotation))))


(defun render-scene (scene &optional (output t))
  (with-slots (pipeline shapes) scene
    (render-shadow-map scene)
    (%render-scene scene #'render-shape output)))
