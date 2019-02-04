(cl:defpackage :cl-bodge.physics.demo
  (:use :cl :bodge-demo.api))
(cl:in-package :cl-bodge.physics.demo)


(defparameter *ball-radius* 5)
(defparameter *ball-position* (ge:vec2 0 15))
(defparameter *ground-position* (list (ge:vec2 -10 5) (ge:vec2 20 -5)))
(defparameter *box-width* 8)
(defparameter *box-height* 16)
(defparameter *box-position* (ge:vec2 8 30))



(defclass 2d-physics-showcase ()
  (universe ground ball box canvas))

(register-showcase '2d-physics-showcase "2D Physics")


(defun on-pre-solve (this that)
  (declare (ignore this that))
  (setf (ge:collision-friction) 1)
  (setf (ge:collision-elasticity) 0.5)
  t)


(defmethod showcase-revealing-flow ((this 2d-physics-showcase) ui)
  (with-slots (universe ground ball box canvas) this
    (ge.ng:>>
     (ge:instantly ()
      (setf universe (ge:make-universe :2d :on-pre-solve #'on-pre-solve)
            ground (ge:make-segment-shape universe
                                          (first *ground-position*)
                                          (second *ground-position*))
            ball (ge:make-circle-shape universe *ball-radius*
                                       :body (ge:make-rigid-body universe))
            box (ge:make-box-shape universe *box-width* *box-height*
                                   :body (ge:make-rigid-body universe)))
      (ge:infuse-circle-mass (ge:shape-body ball) 100 1)
      (ge:infuse-box-mass (ge:shape-body box) 100 10 10)
      (setf (ge:gravity universe) (ge:vec2 0 -9.81)
            (ge:body-position (ge:shape-body ball)) *ball-position*
            (ge:body-position (ge:shape-body box)) *box-position*))
     (ge:for-graphics ()
       (setf canvas (ge:make-canvas '2d-showcase-canvas
                                    (viewport-width) (viewport-height)
                                    :pixel-ratio (viewport-pixel-ratio)))))))


(defmethod showcase-closing-flow ((this 2d-physics-showcase))
  (with-slots (universe ground ball box) this
    (ge:instantly ()
      (let ((body (ge:shape-body ball)))
        (ge:dispose ball)
        (ge:dispose body))
      (let ((body (ge:shape-body box)))
        (ge:dispose box)
        (ge:dispose body))
      (ge:dispose ground)
      (ge:dispose universe))))


(defun rotate-canvas-for (shape)
  (let ((rotation (ge:body-rotation (ge:shape-body shape))))
    (ge:rotate-canvas (atan (ge:y rotation) (ge:x rotation)))))


(defun translate-canvas-for (shape)
  (let ((translation (ge:body-position (ge:shape-body shape))))
    (ge:translate-canvas (ge:x translation) (ge:y translation))))


(defun transform-canvas-for (shape)
  (translate-canvas-for shape)
  (rotate-canvas-for shape))


(defun draw-circle (shape)
  (ge:with-retained-canvas ()
    (transform-canvas-for shape)
    (let* ((r/4 (/ *ball-radius* 4)))
      (ge:draw-circle (ge:vec2 0 0) *ball-radius* :fill-paint (ge:vec4 0 0 0 1))
      (ge:draw-rect (ge:vec2 (- (/ r/4 2)) 0) r/4 (* 3 r/4) :fill-paint (ge:vec4 1 1 1 1)))))


(defun draw-box (shape)
  (ge:with-retained-canvas ()
    (transform-canvas-for shape)
    (ge:draw-rect (ge:vec2 (- (/ *box-width* 2)) (- (/ *box-height* 2)))
                  *box-width* *box-height*
                  :fill-paint (ge:vec4 0 0 0 1))))


(ge:defcanvas 2d-showcase-canvas (ball box)
  (ge:with-retained-canvas ()
    (ge:translate-canvas 300 200)
    (ge:scale-canvas 10 10)
    (draw-circle ball)
    (draw-box box)

    (ge:draw-line (first *ground-position*)
                  (second *ground-position*)
                  (ge:vec4 0 0 0 1)
                  :thickness 0.4)))


(defmethod render-showcase ((this 2d-physics-showcase))
  (with-slots (universe ball canvas box) this
    (ge:render t canvas :ball ball :box box)
    (ge:observe-universe universe 0.020)))
