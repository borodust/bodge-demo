(cl:in-package :cl-bodge.pbr.demo)


(defclass pbr-mesh (ge:disposable)
  ((primitive :reader primitive-of)
   (position-array :reader position-array-of)
   (index-array :reader index-array-of)
   (normal-array :reader normal-array-of)
   (tangent-array :reader tangent-array-of)
   (tex-coord-array :reader tex-coord-array-of)))


(ge:define-destructor pbr-mesh (position-array index-array normal-array tangent-array tex-coord-array)
  (ge:dispose position-array)
  (ge:dispose index-array)
  (ge:dispose normal-array)
  (ge:dispose tangent-array)
  (ge:dispose tex-coord-array))


(defmethod initialize-instance :after ((this pbr-mesh) &key resource)
  (with-slots (position-array index-array normal-array tangent-array tex-coord-array primitive)
      this
    (setf primitive (ge:mesh-resource-primitive resource)
          position-array (ge:make-array-buffer (ge:mesh-resource-position-array resource)
                                               :element-size 3)
          index-array (ge:make-index-buffer (ge:mesh-resource-index-array resource))
          normal-array (ge:make-array-buffer (ge:mesh-resource-normal-array resource)
                                             :element-size 3)
          tangent-array (ge:make-array-buffer (ge:mesh-resource-tangent-array resource)
                                              :element-size 3)
          tex-coord-array (ge:make-array-buffer (ge:mesh-resource-tex-coord-array resource 0)
                                                :element-size 2))))


(defclass pbr-material (ge:disposable) ())


(defmethod initialize-instance :after ((this pbr-material) &key resource)
  (with-slots () this))


(defclass pbr-scene (ge:disposable)
  ((mesh-table :initform (make-hash-table))
   (material-table :initform (make-hash-table))
   (texture-table :initform (make-hash-table :test #'equal))))


(ge:define-destructor pbr-scene (mesh-table material-table texture-table)
  (loop for mesh being the hash-value of mesh-table
        do (ge:dispose mesh))
  (loop for material being the hash-value of material-table
        do (ge:dispose material))
  (loop for texture being the hash-value of texture-table
        do (ge:dispose texture)))


(defmethod initialize-instance :after ((this pbr-scene) &key resource base-path)
  (with-slots (mesh-table material-table texture-table) this
    (ge:do-scene-resource-meshes (mesh id resource)
      (setf (gethash id mesh-table) (make-instance 'pbr-mesh :resource mesh)))
    (ge:do-scene-resource-materials (material id resource)
      (setf (gethash id material-table) (make-instance 'pbr-material :resource material))
      (ge:do-material-resource-textures (texture type id material)
        (let ((texture-name (namestring (ge:texture-resource-name texture))))
          (unless (gethash texture-name texture-table)
            (let ((image (ge:load-resource (fad:merge-pathnames-as-file base-path texture-name))))
              (setf (gethash texture-name texture-table) (ge:make-2d-texture image :rgb)))))))))


(defun scene-texture (scene name)
  (with-slots (texture-table) scene
    (alexandria:if-let ((tex (gethash name texture-table)))
      tex
      (error "Texture '~A' not found" name))))


(defun for-each-scene-mesh (scene fu)
  (with-slots (mesh-table) scene
    (maphash fu mesh-table)))


(defmacro do-scene-meshes ((mesh id scene) &body body)
  `(for-each-scene-mesh ,scene (lambda (,id ,mesh) (declare (ignorable ,id)) ,@body)))
