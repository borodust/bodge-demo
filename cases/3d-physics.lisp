(cl:defpackage :cl-bodge.physics.3d.demo
  (:use :cl :bodge-demo.api :bodge-demo.scene))
(cl:in-package :cl-bodge.physics.3d.demo)


(defclass 3d-physics-showcase ()
  (scene universe bulb
   (objects :initform nil)))
(register-showcase '3d-physics-showcase "3D Physics")


(defclass demo-object (ge:disposable)
  ((drawable :initarg :drawable)
   (shape :initarg :shape)))


(ge:define-destructor demo-object (shape)
  (let ((body (ge:shape-body shape)))
    (ge:dispose shape)
    (ge:dispose body)))


(defun make-demo-object (drawable shape)
  (make-instance 'demo-object :drawable drawable :shape shape))


(defun update-object (object)
  (with-slots (drawable shape) object
    (let ((body (ge:shape-body shape)))
      (update-drawable drawable
                       :transform (ge:mult (ge:vec->translation-mat4-homo (ge:body-position body))
                                           (ge:value->mat4 (ge:body-rotation body) :33 1f0))))))


(defun transform-object (object position-vec rotation-vec)
  (with-slots (shape) object
    (let ((body (ge:shape-body shape)))
      (setf (ge:body-position body) position-vec
            (ge:body-rotation body) (ge:euler-angles->mat3 rotation-vec))
      (update-object object))))


(defun on-pre-solve (this that)
  (declare (ignore this that))
  (setf (ge:collision-friction) 10)
  (setf (ge:collision-elasticity) 0.)
  t)

(defmethod showcase-revealing-flow ((this 3d-physics-showcase) ui)
  (with-slots (scene bulb universe objects) this
    (flet ((%add-sphere (radius position color &key mass emission-color kinematic)
             (let* ((sphere (add-sphere scene))
                    (body (if kinematic
                              (ge.phy:make-kinematic-body universe)
                              (ge.phy:make-rigid-body universe)))
                    (shape (ge.phy:make-sphere-shape universe radius :body body))
                    (object (make-demo-object sphere shape)))
               (update-drawable sphere :color color :emission-color emission-color)
               (unless kinematic
                 (ge.phy:infuse-sphere-mass body mass radius))
               (transform-object object position (ge:vec3))
               (push object objects)))
           (%add-box (x y z position color &key mass kinematic)
             (let* ((box (add-box scene :x x :y y :z z))
                    (body (if kinematic
                              (ge.phy:make-kinematic-body universe)
                              (ge.phy:make-rigid-body universe)))
                    (shape (ge.phy:make-cuboid-shape universe x y z :body body))
                    (object (make-demo-object box shape)))
               (unless kinematic
                 (ge.phy:infuse-cuboid-mass body mass x y z))
               (update-drawable box :color color)
               (transform-object object position (ge:vec3))
               (push object objects))))
    (ge.ng:>>
     (ge:instantly ()
       (setf universe (ge:make-universe :3d :on-pre-solve #'on-pre-solve)
             (ge.phy:gravity universe) (ge:vec3 0 -0.1 0)))
     (ge:for-graphics ()
       (setf scene (make-simple-scene))
       (%add-sphere 1 (ge:vec3 1 1 -1) (ge:vec3 0.2 0.6 0.2) :mass 1)
       (%add-box 1 1 1 (ge:vec3 0 3 -1) (ge:vec3 0.2 0.2 0.6) :mass 2)
       (%add-box 10 0.05 10 (ge:vec3 0 -1.5 -2) (ge:vec3 0.6 0.3 0.4) :kinematic t)

       (update-view scene :position (ge:vec3 1 0 -10) :rotation (ge:vec3 (/ pi 8) 0))
       (let* ((radius 0.1)
              (bulb-drawable (add-sphere scene :radius radius)))
         (update-drawable bulb-drawable :color (ge:vec3 1 1 1)
                                        :emission-color (ge:vec3 0.8 0.8 0.8))
         (setf bulb (make-demo-object
                     bulb-drawable
                     (ge.phy:make-sphere-shape universe radius
                                               :body (ge.phy:make-kinematic-body universe))))))))))


(defmethod showcase-closing-flow ((this 3d-physics-showcase))
  (with-slots (scene universe objects bulb) this
    (ge:for-graphics ()
      (loop for object in objects
            do (ge:dispose object))
      (setf objects nil)
      (ge:dispose bulb)
      (ge:dispose universe)
      (ge:dispose scene))))


(defmethod render-showcase ((this 3d-physics-showcase))
  (with-slots (scene universe bulb objects) this
    (gl:clear-color 0.1 0.1 0.1 1.0)
    (gl:clear :color-buffer)

    (ge:observe-universe universe 0.14)
    (loop for object in objects
          do (update-object object))

    (let ((time (float (bodge-util:real-time-seconds) 0f0)))
      (let ((position (ge:vec3 (* 2 (cos time))
                               0
                               (* 2 (sin time)))))
        (update-light scene :position position)
        (transform-object bulb position (ge:vec3)))
    (render-scene scene))))
