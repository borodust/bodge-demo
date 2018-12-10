(cl:in-package :cl-bodge.pbr.demo)


(bodge-util:define-constant +base-asset-path+ "/bodge/demo/pbr/assets/"
  :test #'equal)


(defun merge-asset-name (&rest names)
  (apply #'fad:merge-pathnames-as-file +base-asset-path+ names))


(defun load-brdf-texture ()
  (ge:make-2d-texture (ge:load-resource (merge-asset-name "brdfLUT.png")
                                        (ge:make-image-resource-handler :png))
                      :rgb))


(defun load-diffuse-ibl-cubemap ()
  (flet ((image-name (suffix)
           (merge-asset-name "papermill/diffuse/"
                             (format nil "diffuse_~A_0.jpg" suffix))))
    (let* ((jpg-handler (ge:make-image-resource-handler :jpeg))
           (back (ge:load-resource (image-name "back") jpg-handler))
           (bottom (ge:load-resource (image-name "bottom") jpg-handler))
           (front (ge:load-resource (image-name "front") jpg-handler))
           (left (ge:load-resource (image-name "left") jpg-handler))
           (right (ge:load-resource (image-name "right") jpg-handler))
           (top (ge:load-resource (image-name "top") jpg-handler)))
      (ge:make-cubemap-texture right top front left bottom back :rgb))))


(defun load-specular-ibl-cubemap ()
  (flet ((image-name (suffix level)
           (merge-asset-name "papermill/specular/"
                             (format nil "specular_~A_~A.jpg" suffix level))))
    (let* ((jpg-handler (ge:make-image-resource-handler :jpeg))
           (back (ge:load-resource (image-name "back" 0) jpg-handler))
           (bottom (ge:load-resource (image-name "bottom" 0) jpg-handler))
           (front (ge:load-resource (image-name "front" 0) jpg-handler))
           (left (ge:load-resource (image-name "left" 0) jpg-handler))
           (right (ge:load-resource (image-name "right" 0) jpg-handler))
           (top (ge:load-resource (image-name "top" 0) jpg-handler))
           (texture (ge:make-cubemap-texture right top front left bottom back :rgb :generate-mipmaps-p nil)))
      (loop for i from 1 to 9
            do (let* ((back (ge:load-resource (image-name "back" i) jpg-handler))
                      (bottom (ge:load-resource (image-name "bottom" i) jpg-handler))
                      (front (ge:load-resource (image-name "front" i) jpg-handler))
                      (left (ge:load-resource (image-name "left" i) jpg-handler))
                      (right (ge:load-resource (image-name "right" i) jpg-handler))
                      (top (ge:load-resource (image-name "top" i) jpg-handler)))
                 (setf (ge:texture-mipmap-level (ge:cubemap-positive-x-layer texture) i) right
                       (ge:texture-mipmap-level (ge:cubemap-positive-y-layer texture) i) top
                       (ge:texture-mipmap-level (ge:cubemap-positive-z-layer texture) i) front
                       (ge:texture-mipmap-level (ge:cubemap-negative-x-layer texture) i) left
                       (ge:texture-mipmap-level (ge:cubemap-negative-y-layer texture) i) bottom
                       (ge:texture-mipmap-level (ge:cubemap-negative-z-layer texture) i) back)))
      texture)))
