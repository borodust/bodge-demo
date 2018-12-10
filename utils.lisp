(cl:in-package :bodge-demo)


(bodge-util:define-constant +cube-vertex-count+ (* 6 6))

;; translated from freeglut implementation of fghCircleTable
(defun generate-circle-table (n half-circle-p)
  (let* ((size (abs n))
         (angle (* (if half-circle-p 1f0 2f0) (/ PI (if (= n 0f0) 1f0 n))))
         (sin-table (make-array (1+ size) :element-type 'single-float :initial-element 0f0))
         (cos-table (make-array (1+ size) :element-type 'single-float :initial-element 0f0)))
    (setf (aref sin-table 0) 0f0
          (aref cos-table 0) 1f0)
    (loop for i from 1 below size
          do (setf (aref sin-table i) (float (sin (* angle i)) 0f0)
                   (aref cos-table i) (float (cos (* angle i)) 0f0)))
    (if half-circle-p
        (setf (aref sin-table size) 0f0
              (aref cos-table size) -1f0)
        (setf (aref sin-table size) (aref sin-table 0)
              (aref cos-table size) (aref cos-table 0)))
    (values sin-table cos-table)))


;; translated from freeglut implementation of fghGenerateSphere
(defun generate-sphere-arrays (radius slices stacks)
  (let* ((vertex-count (+ (* slices (1- stacks)) 2))
         (vertices (make-array (* vertex-count 3) :element-type 'single-float :initial-element 0f0))
         (normals (make-array (* vertex-count 3) :element-type 'single-float :initial-element 0f0))
         (index-count (* 2 (1+ slices) stacks))
         (indicies (make-array index-count :element-type 'fixnum :initial-element 0)))
    (multiple-value-bind (full-sin-table full-cos-table) (generate-circle-table (- slices) nil)
      (multiple-value-bind (half-sin-table half-cos-table) (generate-circle-table stacks t)
        (setf (aref vertices 0) 0f0
              (aref vertices 1) 0f0
              (aref vertices 2) (float radius 0f0)
              (aref normals 0) 0f0
              (aref normals 1) 0f0
              (aref normals 2) 1f0)
        (loop with idx = 3
              for i from 1 below stacks
              do (loop for j from 0 below slices
                       for x = (* (aref full-cos-table j) (aref half-sin-table i))
                       for y = (* (aref full-sin-table j) (aref half-sin-table i))
                       for z = (aref half-cos-table i)
                       do (setf (aref vertices (+ idx 0)) (float (* x radius) 0f0)
                                (aref vertices (+ idx 1)) (float (* y radius) 0f0)
                                (aref vertices (+ idx 2)) (float (* z radius) 0f0)
                                (aref normals (+ idx 0)) (float x 0f0)
                                (aref normals (+ idx 1)) (float y 0f0)
                                (aref normals (+ idx 2)) (float z 0f0))
                          (incf idx 3))
              finally (setf (aref vertices (+ idx 0)) 0f0
                            (aref vertices (+ idx 1)) 0f0
                            (aref vertices (+ idx 2)) (float (- radius) 0f0)
                            (aref normals (+ idx 0)) 0f0
                            (aref normals (+ idx 1)) 0f0
                            (aref normals (+ idx 2)) -1f0))))
    ;; First, generate vertex index arrays for drawing with glDrawElements
    ;; All stacks, including top and bottom are covered with a triangle
    ;; strip.
    (let ((idx 0))
      ;; top stack
      (loop for j from 0 below slices
            do (setf (aref indicies idx) (1+ j)  ; 0 is top vertex, 1 is first for first stack
                     (aref indicies (+ idx 1)) 0)
               (incf idx 2)
            finally (progn
                      (setf (aref indicies idx) 1  ; repeat first slice's idx for closing off shape
                            (aref indicies (+ idx 1)) 0)
                      (incf idx 2)))
      ;; middle stacks:
      ;; Strip indices are relative to first index belonging to strip, NOT
      ;; relative to first vertex/normal pair in array
      (loop for i from 0 below (- stacks 2)
            for offset = (+ 1 (* i slices)) ; triangle_strip indices start at 1 (0 is top vertex),
                                            ; and we advance one stack down as we go along
            do (loop for j from 0 below slices
                     do (setf (aref indicies idx) (+ offset j slices)
                              (aref indicies (+ idx 1)) (+ offset j))
                        (incf idx 2))
               (setf (aref indicies idx) (+ offset slices) ; repeat first slice's idx for
                                                           ; closing off shape
                     (aref indicies (+ idx 1)) offset)
               (incf idx 2))
      ;; bottom stack
      (loop with offset = (+ 1 (- stacks 2) slices) ; triangle_strip indices start at 1 (0 is
                                                    ; top vertex), and we advance one stack down
                                                    ; as we go along
            for j from 0 below slices
            do (setf (aref indicies idx) (- index-count 1) ; zero based index, last element in
                                                           ; array (bottom vertex)...
                     (aref indicies (+ idx 1)) (+ offset j))
               (incf idx 2)
            finally (setf (aref indicies idx) (- index-count 1) ; repeat first slice's idx for
                                                                ; closing off shape
                          (aref indicies (+ idx 1)) offset)))
    (values vertices normals indicies)))


(defun make-box-vertex-array (x y z)
  (let* ((x (float (/ x 2) 0f0))
         (y (float (/ y 2) 0f0))
         (z (float (/ z 2) 0f0))
         (-x (- x))
         (-y (- y))
         (-z (- z)))
    (make-array +cube-vertex-count+ :element-type 'ge:vec3
                                    :initial-contents (list (ge:vec3 x -y z) ; front
                                                            (ge:vec3 x y z)
                                                            (ge:vec3 -x -y z)
                                                            (ge:vec3 -x -y z)
                                                            (ge:vec3 x y z)
                                                            (ge:vec3 -x y z)

                                                            (ge:vec3 -x -y z) ; left
                                                            (ge:vec3 -x y z)
                                                            (ge:vec3 -x -y -z)
                                                            (ge:vec3 -z -y -z)
                                                            (ge:vec3 -x y z)
                                                            (ge:vec3 -x y -z)

                                                            (ge:vec3 -x y -z) ; back
                                                            (ge:vec3 x y -z)
                                                            (ge:vec3 x -y -z)
                                                            (ge:vec3 x -y -z)
                                                            (ge:vec3 -x -y -z)
                                                            (ge:vec3 -x y -z)

                                                            (ge:vec3 x -y -z) ; right
                                                            (ge:vec3 x y -z)
                                                            (ge:vec3 x y z)
                                                            (ge:vec3 x y z)
                                                            (ge:vec3 x -y z)
                                                            (ge:vec3 x -y -z)

                                                            (ge:vec3 x -y z) ; bottom
                                                            (ge:vec3 -x -y z)
                                                            (ge:vec3 x -y -z)
                                                            (ge:vec3 x -y -z)
                                                            (ge:vec3 -x -y z)
                                                            (ge:vec3 -x -y -z)

                                                            (ge:vec3 x y z) ; top
                                                            (ge:vec3 x y -z)
                                                            (ge:vec3 -x y -z)
                                                            (ge:vec3 -x y -z)
                                                            (ge:vec3 -x y z)
                                                            (ge:vec3 x y z)))))

(defun make-box-normal-array ()
  (make-array +cube-vertex-count+ :element-type 'ge:vec3
                      :initial-contents (list (ge:vec3 0 0 1)
                                              (ge:vec3 0 0 1)
                                              (ge:vec3 0 0 1)
                                              (ge:vec3 0 0 1)
                                              (ge:vec3 0 0 1)
                                              (ge:vec3 0 0 1)

                                              (ge:vec3 -1 0 0)
                                              (ge:vec3 -1 0 0)
                                              (ge:vec3 -1 0 0)
                                              (ge:vec3 -1 0 0)
                                              (ge:vec3 -1 0 0)
                                              (ge:vec3 -1 0 0)

                                              (ge:vec3 0 0 -1)
                                              (ge:vec3 0 0 -1)
                                              (ge:vec3 0 0 -1)
                                              (ge:vec3 0 0 -1)
                                              (ge:vec3 0 0 -1)
                                              (ge:vec3 0 0 -1)

                                              (ge:vec3 1 0 0)
                                              (ge:vec3 1 0 0)
                                              (ge:vec3 1 0 0)
                                              (ge:vec3 1 0 0)
                                              (ge:vec3 1 0 0)
                                              (ge:vec3 1 0 0)

                                              (ge:vec3 0 -1 0)
                                              (ge:vec3 0 -1 0)
                                              (ge:vec3 0 -1 0)
                                              (ge:vec3 0 -1 0)
                                              (ge:vec3 0 -1 0)
                                              (ge:vec3 0 -1 0)


                                              (ge:vec3 0 1 0)
                                              (ge:vec3 0 1 0)
                                              (ge:vec3 0 1 0)
                                              (ge:vec3 0 1 0)
                                              (ge:vec3 0 1 0)
                                              (ge:vec3 0 1 0))))

(defun generate-box-arrays (x y z)
  (let ((indices (make-array +cube-vertex-count+ :element-type 'fixnum :initial-element 0)))
    (loop for i from 0 below +cube-vertex-count+
          do (setf (aref indices i) i))
    (values (make-box-vertex-array x y z)
            (make-box-normal-array)
            indices)))


;;;
;;; CUBEMAP BANNER
;;;
