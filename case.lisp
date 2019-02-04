(cl:in-package :bodge-demo.api)

(defvar *viewport-pixel-ratio* 1f0)
(defvar *viewport-scale* 1f0)
(defvar *viewport-width*)
(defvar *viewport-height*)
(defvar *loading-screen-canvas*)


(defun viewport-width ()
  *viewport-width*)


(defun viewport-height ()
  *viewport-height*)


(defun viewport-pixel-ratio ()
  *viewport-pixel-ratio*)


(defun merge-showcase-pathname (pathname)
  (merge-pathnames pathname
                   (merge-pathnames "cases/"
                                    (asdf:component-pathname (asdf:find-system :bodge-demo)))))


(defvar *showcases* nil)

(defun register-showcase (class name)
  (pushnew (cons class name) *showcases* :key #'car))

(defun list-showcases ()
  (reverse *showcases*))


(defun showcase-class (showcase)
  (car showcase))

(defun showcase-name (showcase)
  (cdr showcase))

(defgeneric showcase-revealing-flow (case-manager ui))
(defgeneric showcase-closing-flow (case-manager))

(defgeneric render-showcase (case-manager)
  (:method (case-manager) (declare (ignore case-manager))))


(ge:defcanvas loading-screen (color)
  (ge:translate-canvas 310 300)
  (labels ((wave (shift)
             (let ((time (* (bodge-util:epoch-seconds) 4)))
               (* (cos (* (+ time shift) 6)) 2)))
           (draw-letter (shift letter)
             (ge:draw-text (ge:vec2 (* 10 shift) (wave shift)) letter color)))
    (ge:scale-canvas 2 2)
    (loop for letter across #("L" "O" "A" "D" "I" "N" "G")
          for i from 0
          do (draw-letter i letter))))


(defun ~init-api (width height &key (pixel-ratio 1f0))
  (ge:>>
   (ge:for-graphics ()
     (setf *viewport-width* width
           *viewport-height* height
           *viewport-pixel-ratio* pixel-ratio
           *loading-screen-canvas* (ge:make-canvas 'loading-screen width height
                                                   :pixel-ratio pixel-ratio)))))



(defun ~destroy-api ()
  (ge:>>
   (ge:instantly ()
     (ge:dispose *loading-screen-canvas*))))


(defun render-loading-screen (color)
  (ge:render t *loading-screen-canvas* :color color))
