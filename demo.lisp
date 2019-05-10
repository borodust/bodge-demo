(cl:in-package :bodge-demo)


(declaim (special *demo*))


(defparameter *viewport-width* 800)
(defparameter *viewport-height* 600)


(ge:defapp demo ()
  ((active-showcase :initform nil)
   (next-showcase :initform nil))
  (:panels 'main-menu)
  (:viewport-width *viewport-width*)
  (:viewport-height *viewport-height*)
  (:default-initargs :depends-on '(ge:host-system ge:physics-system
                                   ge:graphics-system ge:audio-system)))



(defmethod ge:draw ((this demo))
  (with-slots (active-showcase) this
    (ge:clear-rendering-output t)
    (when active-showcase
      (render-showcase active-showcase))))


(ge:defpanel (main-menu
              (:title "Main Menu")
              (:width 150) (:height *viewport-height*)
              (:options :scrollable)))


(defmethod initialize-instance :after ((this main-menu) &key)
  (loop for case-object in (list-showcases)
        do (let ((showcase-class (showcase-class case-object))
                 (showcase-name (showcase-name case-object)))
             (flet ((switch-showcase (win)
                      (declare (ignore win))
                      (with-slots (next-showcase) (ge:app)
                        (setf next-showcase (make-instance showcase-class)))))
               (let ((button (make-instance 'ge:button
                                            :label showcase-name
                                            :on-click #'switch-showcase)))
                 (ge:adopt this button))))))


(defun cleanup-flow (this)
  (with-slots (active-showcase) this
    (prog1 (when active-showcase
             (showcase-closing-flow active-showcase))
      (setf active-showcase nil))))


(defun ~switch-showcase-if-requested (this)
  (with-slots (active-showcase next-showcase) this
    (when next-showcase
      (ge:>>
       (cleanup-flow this)
       (showcase-revealing-flow next-showcase (ge:app-ui))
       (ge:instantly ()
         (setf active-showcase next-showcase
               next-showcase nil))))))


(defmethod ge:acting-flow ((this demo))
  (~switch-showcase-if-requested this))


(defmethod ge:configuration-flow ((this demo))
  (~init-api *viewport-width* *viewport-height*))


(defmethod ge:sweeping-flow ((this demo))
  (~destroy-api))


(defun run (&key (log-level :info))
  (ge.app:start 'demo :log-level log-level))
