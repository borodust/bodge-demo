(cl:defpackage :cl-bodge.physics.framebuffers.demo
  (:use :cl :bodge-demo.api :bodge-demo.scene))
(cl:in-package :cl-bodge.physics.framebuffers.demo)


(defclass framebuffers-showcase () ())


(register-showcase 'framebuffers-showcase "Framebuffers")


(defvar *box* nil)
(defvar *box-pipeline* nil)

(defvar *scene-1* nil)
(defvar *scene-box* nil)
(defvar *scene-2* nil)
(defvar *scene-ball* nil)

(defvar *framebuffer* nil)

(defvar *banner* nil)

(defvar *2d-texture* nil)
(defvar *depth-texture* nil)
(defvar *cubemap-texture* nil)
(defvar *depth-cubemap-texture* nil)

(defparameter *view-matrix* (ge:translation-mat4-homo 0 0 -1))
(defparameter *projection-matrix* (ge:perspective-projection-mat 1 (/ 480 640) 1 10))


(ge:defshader (framebuffers-shader
               (:sources "framebuffers.glsl")
               (:base-path (merge-showcase-pathname "framebuffers/")))
  (position :name "vPosition")
  (cube-texture :name "cubeMap")
  (model :name "model")
  (view :name "view")
  (projection :name "projection"))


(ge:defpipeline (framebuffers-pipeline
                 (:primitive :triangles))
  :vertex framebuffers-shader
  :fragment framebuffers-shader)


(defclass box (ge:disposable)
  ((position-buffer :initarg :position-buffer)
   (index-buffer :initarg :index-buffer)
   (color :initform (ge:vec3 0.9 0.9 0.9))))


(ge:define-destructor box (position-buffer index-buffer)
  (ge:dispose position-buffer)
  (ge:dispose index-buffer))


(defun render-box (box output transform)
  (with-slots (position-buffer index-buffer) box
    (ge:render output *box-pipeline*
               :index-buffer index-buffer
               'position position-buffer
               'model transform
               'view *view-matrix*
               'cube-texture *cubemap-texture*
               'projection *projection-matrix*)))


(defun make-box (x y z)
  (multiple-value-bind (vertices normals indices)
      (bodge-demo::generate-box-arrays x y z)
    (declare (ignore normals))
    (make-instance 'box :index-buffer (ge:make-index-buffer indices)
                        :position-buffer (ge:make-array-buffer vertices :element-size 3))))


(defun init-scene-1 ()
  (setf *scene-1* (make-simple-scene
                   :projection-matrix (ge:perspective-projection-mat 1 1 1 10))
        *scene-box* (add-box *scene-1*))
  (update-light *scene-1* :position (ge:vec3 2 -2 4))
  (update-drawable *scene-box* :color (ge:vec3 0 1 0)))


(defun render-scene-1 (output time)
  (ge:clear-rendering-output output :color (ge:vec4 0.2 0.2 0.2 1))
  (update-drawable *scene-box*
                   :transform (ge:mult
                               (ge:translation-mat4-homo (* 0.5 (sin time))
                                                         (* 0.5 (cos time))
                                                         -3)
                               (ge:euler-angles->mat4-homo (ge:vec3 time (sin time) time))))
  (render-scene *scene-1* output))


(defun init-scene-2 ()
  (setf *scene-2* (make-simple-scene
                   :projection-matrix (ge:perspective-projection-mat 1 1 1 10))
        *scene-ball* (add-sphere *scene-2*))
  (update-light *scene-2* :position (ge:vec3 -5 2 2))
  (update-drawable *scene-ball* :color (ge:vec3 1 0 0)))


(defun render-scene-2 (output time)
  (ge:clear-rendering-output output :color (ge:vec4 0.2 0.2 0.2 1))
  (update-drawable *scene-ball* :transform (ge:translation-mat4-homo (* 0.9 (cos (* time 2)))
                                                                     (* 0.9 (sin (* time 2)))
                                                                     -4.5))
  (render-scene *scene-2* output))


(defmethod showcase-revealing-flow ((this framebuffers-showcase) ui)
  (ge:for-graphics ()
    (init-scene-1)
    (init-scene-2)
    (setf *box* (make-box 1 1 1)
          *box-pipeline* (ge:make-shader-pipeline 'framebuffers-pipeline)
          *framebuffer* (ge:make-framebuffer)
          *banner* (ge:make-2d-banner -1 -1 2 2)

          *2d-texture* (ge:make-empty-2d-texture 640 480 :rgba)
          *depth-texture* (ge:make-empty-depth-texture 640 480)
          *cubemap-texture* (ge.gx:make-empty-cubemap-texture 1024 :rgba)
          *depth-cubemap-texture* (ge.gx:make-empty-depth-cubemap-texture 1024))
    (ge.gx:configure-framebuffer *framebuffer* *2d-texture* *depth-texture*)))


(defmethod showcase-closing-flow ((this framebuffers-showcase))
  (ge:for-graphics ()
    (loop for element in (list *box*
                               *box-pipeline*
                               *scene-1*
                               *scene-2*
                               *framebuffer*
                               *banner*
                               *2d-texture*
                               *depth-texture*
                               *cubemap-texture*
                               *depth-cubemap-texture*)
          do (ge:dispose element))))


(defmethod render-showcase ((this framebuffers-showcase))
  (ge:clear-rendering-output t :color (ge:vec4 0.1 0.1 0.1 1.0))
  (ge:clear-rendering-output *cubemap-texture*
                             :color (ge:vec4 0.8 0.8 0.8 1.0))


  (let ((time (float (bodge-util:real-time-seconds) 0f0)))
    ;; box scene with implicit framebuffer
    (ge:clear-rendering-output *depth-texture*)
    (render-scene-1 (ge:cubemap-positive-x-layer *cubemap-texture*) time)
    (render-scene-1 *depth-texture* time)
    (ge:render-banner *banner* *depth-texture* :output (ge:cubemap-positive-y-layer *cubemap-texture*))

    ;; sphere scene with explicit framebuffer
    (ge:clear-rendering-output *depth-texture*)
    (render-scene-2 *framebuffer* time)
    (ge:render-banner *banner* *2d-texture*
                      :output (ge:cubemap-negative-x-layer *cubemap-texture*))
    (ge:render-banner *banner* *depth-texture*
                      :output (ge:cubemap-negative-y-layer *cubemap-texture*))

    (ge:clear-rendering-output (ge:cubemap-positive-z-layer *cubemap-texture*)
                               :color (ge:vec4 (+ 0.4 (* 0.3 (cos time)))
                                               (+ 0.4 (* 0.3 (sin time)))
                                               0.5
                                               1))
    (ge:clear-rendering-output (ge:cubemap-negative-z-layer *cubemap-texture*)
                               :color (ge:vec4 (+ 0.4 (* 0.3 (sin time)))
                                               (+ 0.4 (* 0.3 (cos time)))
                                               0.5
                                               1))

    ;; main box
    (render-box *box* t (ge:mult (ge:translation-mat4-homo 0.3 0 -2)
                                 (ge:euler-angles->mat4-homo (ge:vec3 (* 0.2 time)
                                                                      (* 0.4 time)
                                                                      (* 0.6 time)))))))
