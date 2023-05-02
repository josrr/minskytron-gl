;;;; minskytron-gl.asd

(asdf:defsystem #:minskytron-gl
  :description "Describe minskytron-gl here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop
               #:alexandria
               #:bordeaux-threads
               #:local-time
               #:log4cl
               #:cl-opengl
               #:opticl
               #:rtg-math
               #:mathkit
               #:sdl2)
  :components ((:file "package")
               (:file "shaders")
               (:file "minskytron")
               (:file "minskytron-gl")))
