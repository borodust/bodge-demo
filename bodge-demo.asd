(asdf:defsystem bodge-demo
  :description "bodge-demo to check if all systems work as expected"
  :version "1.0.0"
  :author "Pavel Korolev"
  :license "MIT"
  :depends-on (cl-bodge)
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "scene")
               (:file "case")
               (:module cases
                :serial t
                :components ((:file "2d-physics")
                             (:file "3d-physics")
                             (:file "framebuffers/framebuffers")
                             (:file "ui")
                             (:file "text/text")
                             (:module pbr
                              :serial t
                              :components ((:file "packages")
                                           (:file "utils")
                                           (:file "scene")
                                           (:file "pbr")))
                             (:file "animation/animation")
                             (:file "test/test")))
               (:file "demo")))
