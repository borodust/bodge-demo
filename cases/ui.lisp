(cl:defpackage :cl-bodge.ui.demo
  (:use :cl :bodge-demo.api))
(cl:in-package :cl-bodge.ui.demo)


(defclass custom-button (ge:custom-widget) ())


(defmethod ge:render-custom-widget ((this custom-button) origin width height)
  (ge:draw-rect origin width height
                :fill-paint (cond
                              ((and (ge:custom-widget-hovered-p this)
                                    (ge:custom-widget-pressed-p this :left))
                               (ge:vec4 0.3 0.3 0.3 1.0))
                              ((ge:custom-widget-hovered-p this) (ge:vec4 0.5 0.5 0.5 1.0))
                              (t (ge:vec4 0.0 0.0 0.0 1.0))))
  (ge:draw-text (ge:add origin (ge:vec2 12 9)) "Hello Widget" (ge:vec4 1.0 1.0 1.0 1.0)))


(let ((output *standard-output*))
  (defun on-hover (window &key)
    (declare (ignore window))
    (format output "~&hovering"))
  (defun on-leave (window &key)
    (declare (ignore window))
    (format output "~&leaving"))
  (defun on-click (window &key button)
    (declare (ignore window))
    (format output "~&~A clicked" button))
  (defun on-mouse-press (window &key button)
    (declare (ignore window))
    (format output "~&~A pressed" button))
  (defun on-mouse-release (window &key button)
    (declare (ignore window))
    (format output "~&~A released" button)))


(defclass ui-demo-window-model ()
  ((ui :initarg :ui)))


(ge:defpanel (ui-demo-window
              (:title "UI Demo")
              (:origin 200 50)
              (:width 400) (:height 400)
              (:inherit ui-demo-window-model)
              (:options :movable :resizable
                        :minimizable :scrollable
                        :closable))
  (ge:label :text "Nested:")
  (ge:horizontal-layout
   (ge:radio-group
    (ge:radio :label "Option 1")
    (ge:radio :label "Option 2" :activated t))
   (ge:vertical-layout
    (ge:check-box :label "Check 1" :width 100)
    (ge:check-box :label "Check 2"))
   (ge:vertical-layout
    (ge:label :text "Awesomely" :align :left)
    (ge:label :text "Stacked" :align :centered)
    (ge:label :text "Labels" :align :right)))
  (ge:label :text "Expand by width:")
  (ge:horizontal-layout
   (ge:button :label "Dynamic")
   (ge:button :label "Min-Width" :width 80)
   (ge:button :label "Fixed-Width" :expandable nil :width 100))
  (ge:label :text "Expand by ratio:")
  (ge:horizontal-layout
   (ge:button :label "1.0" :expand-ratio 1.0)
   (ge:button :label "0.75" :expand-ratio 0.75)
   (ge:button :label "0.5" :expand-ratio 0.5))
  (ge:label :text "Rest:")
  (ge:button :label "Top-Level Button")
  (custom-button :on-hover #'on-hover
                 :on-leave #'on-leave
                 :on-click #'on-click
                 :on-mouse-press #'on-mouse-press
                 :on-mouse-release #'on-mouse-release))


(defmethod ge:on-close ((this ui-demo-window))
  (with-slots (ui) this
    (ge:remove-panel ui this)))


;;;
;;; UI Showcase
;;;
(defclass ui-showcase ()
  ((window :initform nil)
   (ui :initform nil)))
(register-showcase 'ui-showcase "UI")


(defmethod showcase-revealing-flow ((this ui-showcase) ui)
  (with-slots (window (this-ui ui)) this
    (ge:instantly ()
      (ge:with-ui-access (ui)
        (setf this-ui ui
              window (ge:add-panel ui 'ui-demo-window :ui ui))))))


(defmethod showcase-closing-flow ((this ui-showcase))
  (with-slots (window ui) this
    (ge:instantly ()
      (ge:with-ui-access (ui)
        (ge:remove-panel ui window)
        (setf window nil
              ui nil)))))
