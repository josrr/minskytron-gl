;;;; minskytron-gl.asd

(asdf:defsystem #:minskytron-gl
  :description "Minskytron implementation using SDL and OpenGL"
  :author "José M. Á. Ronquillo Rivera <josrr@ymail.com>"
  :license  "GPLv3"
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
