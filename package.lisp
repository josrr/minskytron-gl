;;;; package.lisp

(defpackage #:shaders
  (:use #:cl)
  (:export #:with-shaders
	   #:compile-shaders
           #:read-shader-from-file))

(defpackage #:minskytron-gl
  (:use #:cl #:cl-opengl))
