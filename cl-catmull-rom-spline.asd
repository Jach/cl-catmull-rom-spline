(asdf:defsystem #:cl-catmull-rom-spline
  :description "Catmull-Rom Spline"
  :version "0.1.0"
  :author "Kevin Secretan <github@thejach.com>"
  :license "Public Domain"
  :serial t
  :pathname "src"
  :components ((:file "spline"))
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-catmull-rom-spline/test))))

(asdf:defsystem #:cl-catmull-rom-spline/test
  :depends-on (#:cl-catmull-rom-spline
               #:fiveam
               #:arrow-macros
               #:uiop)
  :serial t
  :pathname "test"
  :components ((:file "spline-test"))
  :perform (asdf:test-op (o c) (uiop:symbol-call ':5am '#:run-all-tests ':summary ':suite)))

(asdf:defsystem #:cl-catmull-rom-spline/example
  :depends-on (#:cl-catmull-rom-spline
               #:bordeaux-threads
               #:lispbuilder-sdl
               #:lispbuilder-sdl-ttf
               #:lispbuilder-sdl-gfx)
  :serial t
  :pathname "example"
  :components ((:file "spline-example")))
