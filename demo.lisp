(cl:in-package :bodge-demo)


(declaim (special *demo*))


(defvar *ui-width* 640)
(defvar *ui-height* 480)


(defclass demo (ge:enableable ge:generic-system)
  (ui
   (active-showcase :initform nil)
   (next-showcase :initform nil)
   (task-queue :initform (ge:make-task-queue) :reader %task-queue-of))
  (:default-initargs :depends-on '(ge:host-system ge:physics-system
                                   ge:graphics-system ge:audio-system)))


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


(defun render (demo)
  (with-slots (ui active-showcase) demo
    (ge:clear-rendering-output t)
    (when active-showcase
      (render-showcase active-showcase))
    (ge:compose-ui ui)
    (ge:swap-buffers)))


(ge:defpanel (main-menu
              (:title "Main Menu")
              (:width 150) (:height 480)
              (:options :scrollable)))


(defun push-task (task)
  (alexandria:when-let (demo (ge:engine-system 'demo nil))
    (ge:push-task task (%task-queue-of demo))))


(defun init-graphics (this)
  (with-slots (ui next-showcase) this
    (let ((input-source (ge:make-host-input-source)))
      (ge:attach-host-input-source input-source)
      (setf ui (ge:make-ui *ui-width* *ui-height*
                           :pixel-ratio *viewport-pixel-ratio*
                           :input-source input-source)
            *loading-screen-canvas* (ge:make-canvas 'loading-screen *ui-width* *ui-height*))
      (let ((main-window (ge:add-panel ui 'main-menu :origin (ge:vec2 100 100))))
        (loop for case-object in (list-showcases)
              do (let ((showcase-class (showcase-class case-object)))
                   (flet ((switch-showcase (win)
                            (declare (ignore win))
                            (setf next-showcase (make-instance showcase-class))))
                     (let ((button (make-instance 'ge:button
                                                  :label (showcase-name case-object)
                                                  :on-click #'switch-showcase)))
                       (ge:adopt main-window button)))))))))


(defun scale-viewport ()
  (let ((scaled-viewport-size (ge:mult (ge:viewport-size) (ge:viewport-scale))))
    (setf (ge:viewport-size) scaled-viewport-size)))


(defun init-host (this)
  (declare (ignore this))
  (scale-viewport)
  (let ((viewport-size (ge:viewport-size))
        (framebuffer-size (ge:framebuffer-size)))
    (setf (ge:swap-interval) 1
          *viewport-pixel-ratio* (bodge-util:f (/ (ge:x viewport-size) (ge:x framebuffer-size)))
          *viewport-scale* (bodge-util:f (/ *ui-width* (ge:x viewport-size))))))


#|
(defun update-ui-scale (demo scale)
  (with-slots (ui) demo
    (ge:update-ui-scale ui (setf *viewport-scale* scale))
    (ge:update-ui-pixel-ratio ui (* (/ *viewport-scale*) *viewport-pixel-ratio*))))


(defun update-ui-pixel-ratio (demo pixel-ratio)
  (with-slots (ui) demo
    (setf *viewport-pixel-ratio* (* (/ *viewport-scale*) pixel-ratio))
    (ge:update-ui-pixel-ratio ui *viewport-pixel-ratio*)
    (ge:update-canvas-pixel-ratio *loading-screen-canvas* *viewport-pixel-ratio*)))


(ge:define-event-handler on-window-size-change ((evt ge:viewport-size-change-event) width height)
  (ge:run
   (ge:for-host ()
     (let ((scale (bodge-util:f (/ *ui-width* width))))
       (push-task (lambda () (update-ui-scale *demo* scale)))))))


(ge:define-event-handler on-framebuffer-size-change ((evt ge:framebuffer-size-change-event) width height)
  (ge:run
   (ge:for-host ()
     (let ((pixel-ratio (bodge-util:f (/ (ge:x (ge:viewport-size)) width))))
       (push-task (lambda () (update-ui-pixel-ratio *demo* pixel-ratio)))))))

|#

(defun cleanup-flow (this)
  (with-slots (active-showcase) this
    (prog1 (when active-showcase
             (showcase-closing-flow active-showcase))
      (setf active-showcase nil))))


(defun switch-showcase-if-requested (this)
  (with-slots (active-showcase next-showcase ui) this
    (when next-showcase
      (ge:>>
       (cleanup-flow this)
       (showcase-revealing-flow next-showcase ui)
       (ge:instantly ()
         (setf active-showcase next-showcase
               next-showcase nil))))))


(defun cleanup-demo (this)
  (with-slots (ui) this
    (ge:dispose ui)
    (ge:dispose *loading-screen-canvas*)))


(defmethod ge:initialize-system :after ((this demo))
  (ge:run (ge:>>
           (ge:for-host ()
             (init-host this))
           (ge:for-graphics ()
             (init-graphics this))
           (ge:loop-flow
            (ge:>>
             (ge:instantly ()
               (flet ((invoke (fn)
                        (declare (type function fn))
                        (let ((*demo* this))
                          (funcall fn))))
                 (ge:drain (%task-queue-of this) #'invoke)))
             (ge:for-graphics ()
               (render this))
             (ge:->> ()
               (switch-showcase-if-requested this)))
            (lambda () (ge:enabledp this)))
           (ge:->> ()
             (cleanup-flow this))
           (ge:instantly ()
             (cleanup-demo this)))))


(ge:define-event-handler exit-handler ((eve ge:viewport-hiding-event))
  (ge:shutdown))


(defun run (&key (log-level :info))
  (ge:startup `(:engine (:systems (demo)
                         :log-level ,log-level)
                :host (:opengl-version (3 3)))))
