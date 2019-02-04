(cl:defpackage :bodge-demo.api
  (:use :cl)
  (:export #:~init-api
           #:~destroy-api
           #:viewport-pixel-ratio
           #:viewport-width
           #:viewport-height
           #:render-loading-screen
           #:register-showcase
           #:list-showcases
           #:showcase-name
           #:showcase-class
           #:showcase-revealing-flow
           #:showcase-closing-flow
           #:render-showcase
           #:merge-showcase-pathname))


(cl:defpackage :bodge-demo
  (:use :cl :bodge-demo.api)
  (:export #:run))
