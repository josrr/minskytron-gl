;;;; package.lisp

(defpackage #:shaders
  (:use #:cl #:cl-opengl)
  (:export #:con-shader
	   #:compila-shader
           #:lee-shader-de-archivo))

(defpackage #:minskytron-gl
  (:use #:cl #:cl-opengl))
