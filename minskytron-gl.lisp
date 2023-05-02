;;;; minskytron-gl.lisp

(in-package #:minskytron-gl)

(setf (uiop/os:getenv "MESA_GL_VERSION_OVERRIDE") "3.3")
(setf (uiop/os:getenv "MESA_GLSL_VERSION_OVERRIDE") "330")

(defclass obj ()
  ((vbo :initform (gen-buffer) :reader obj-vbo)
   (vao :initform (gen-vertex-array) :reader obj-vao)
   (program :initarg :program :accessor obj-program)
   (pos :initarg :pos :accessor obj-pos)))

(defparameter *minskytron-v-shader*
  "#version 330 core
in vec2 posicion;

out vec3 Color;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;
uniform vec3 color;

void main() {
  gl_PointSize=3.0;
  Color = color;
  gl_Position = proj*view*model*vec4(posicion, 0.0, 1.0);
}")

(defparameter *minskytron-f-shader*
  "#version 330 core
in vec3 Color;
out vec4 outColor;

void main() {
  outColor = vec4(Color, 1.0);
}")

(defclass minskytron (obj)
  ((num-points :initform 128 :initarg :num-points
               :accessor minskytron-num-points)
   (fb :initform (gen-framebuffer) :reader minskytron-fb)
   (bf :initform (gen-texture) :reader minskytron-bf)
   (points :initarg :points :accessor minskytron-points)
   (words :initform nil :initarg :words :accessor minskytron-words)
   (u-model :initarg :model :accessor minskytron-u-model)
   (u-view :initarg :view :accessor minskytron-u-view)
   (u-proy :initarg :proy :accessor minskytron-u-proy)
   (u-color :initarg :color :accessor minskytron-u-color)
   (data :initarg :data :accessor minskytron-data)))

(defun make-minskytron (num-points width height)
  (let* ((program (shaders:compila-shader *minskytron-v-shader*
                                          *minskytron-f-shader*))
         (med-width (/ width 2f0))
         (med-height (/ height 2f0))
         (*first-time* t)
         (m (make-instance 'minskytron
                           :num-points num-points
                           :program program
                           :pos (get-attrib-location program "posicion")
                           :points (alloc-gl-array :float (* num-points 6))
                           :model (get-uniform-location program "model")
                           :view (get-uniform-location program "view")
                           :proy (get-uniform-location program "proj")
                           :color (get-uniform-location program "color")
                           :data (gen-minskytron-pars))))
    (bind-vertex-array (obj-vao m))
    (bind-frag-data-location program 0 "outColor")
    (uniform-matrix-4fv (minskytron-u-view m) (rtg-math.matrix4:identity) nil)
    (uniform-matrix-4fv (minskytron-u-proy m)
                        (kit.math:ortho-matrix (- med-width) med-width
                                               (- med-height) med-height
                                               0.0001 3000.0)
                        nil)
    (uniform-matrix-4fv (minskytron-u-model m) (rtg-math.matrix4:identity) nil)
    (uniformf (minskytron-u-color m) 0.3 0.9 0.8)
    (bind-buffer :array-buffer (obj-vbo m))
    (enable-vertex-attrib-array (obj-pos m))
    (vertex-attrib-pointer (obj-pos m) 2 :float :false 8 0)
    (bind-framebuffer :draw-framebuffer (minskytron-fb m))
    (bind-texture :texture-2d (minskytron-bf m))
    (tex-image-2d :texture-2d 0 :rgba width height 0 :rgba :unsigned-byte nil)
    (tex-parameter :texture-2d :texture-min-filter :linear)
    (tex-parameter :texture-2d :texture-mag-filter :linear)
    (bind-texture :texture-2d 0)
    (framebuffer-texture-2d :framebuffer :color-attachment0 :texture-2d
                            (minskytron-bf m) 0)
    m))

(defmethod draw-obj ((obj minskytron) &optional texture)
  (declare (ignore texture))
  (bind-framebuffer :framebuffer (minskytron-fb obj))
  (use-program (obj-program obj))
  (bind-vertex-array (obj-vao obj))
  (opengl:clear :color-buffer-bit)
  (gen-minskytron (minskytron-points obj)
                  (minskytron-data obj)
                  (minskytron-num-points obj))
  (buffer-data :array-buffer :static-draw (minskytron-points obj))
  (draw-arrays :points 0 (* 3 (minskytron-num-points obj)))
  (minskytron-bf obj))

(defun minskytron-restart (minskytron)
  (clear-color 0.0 0.0 0.0 1.0)
  (opengl:clear :color-buffer-bit)
  (clear-color 0.0 0.0 0.0 0.075)
  (setf (minskytron-data minskytron) (gen-minskytron-pars (minskytron-data minskytron))))

(defun minskytron-read-words (minskytron scancode)
  (push (1+ (parse-integer (sdl2:scancode-name scancode)))
        (minskytron-words minskytron))
  (when (= (length (minskytron-words minskytron)) 6)
    (let ((words (reverse (minskytron-words minskytron))))
      (format *debug-io* "words: ~S~%" words)
      (clear-color 0.0 0.0 0.0 1.0)
      (opengl:clear :color-buffer-bit)
      (clear-color 0.0 0.0 0.0 0.075)
      (setf (minskytron-data minskytron) (gen-minskytron-pars words)))
    (setf (minskytron-words minskytron) nil)))

(defparameter *quad*
  (let ((arreglo (alloc-gl-array :float 16)))
    (loop for i from 0 and valor in '(-1.0  1.0 0.0 1.0
				       1.0  1.0 1.0 1.0
                                       1.0 -1.0 1.0 0.0
                                      -1.0 -1.0 0.0 0.0)
          do (setf (glaref arreglo i) valor))
    arreglo))

(defparameter *quad-elems*
  (let ((arreglo (alloc-gl-array :uint 6)))
    (loop for i from 0 and valor in '(0 1 2
                                      2 3 0)
          do (setf (glaref arreglo i) valor))
    arreglo))

(defparameter *quad-v-shader*
  "#version 330 core
layout (location = 0) in vec2 aPos;
layout (location = 1) in vec2 aTexCoords;

out vec2 TexCoords;

void main()
{
    gl_Position = vec4(aPos.x, aPos.y, 0.0, 1.0);
    TexCoords = aTexCoords;
}")

(defparameter *quad-f-shader*
  "#version 330 core
out vec4 FragColor;

in vec2 TexCoords;

uniform sampler2D screenTexture;

void main()
{
    FragColor = texture(screenTexture, TexCoords);
}")

(defclass quad (obj)
  ((ebo :initform (gen-buffer) :reader quad-ebo)
   (tex-coords :initarg :tex-coords :accessor quad-tex-coords)))

(defun make-quad ()
  (let* ((program (shaders:compila-shader *quad-v-shader* *quad-f-shader*))
         (quad (make-instance 'quad
                              :program program
                              :pos (get-attrib-location program "aPos")
                              :tex-coords (get-attrib-location program "aTexCoords"))))
    (bind-vertex-array (obj-vao quad))
    (bind-buffer :element-array-buffer (quad-ebo quad))
    (bind-buffer :array-buffer (obj-vbo quad))
    (bind-frag-data-location program 0 "FragColor")
    (buffer-data :element-array-buffer :static-draw *quad-elems*)
    (buffer-data :array-buffer :static-draw *quad*)
    (vertex-attrib-pointer        (obj-pos quad) 2 :float :false 16 0)
    (vertex-attrib-pointer (quad-tex-coords quad) 2 :float :false 16 8)
    (enable-vertex-attrib-array (obj-pos quad))
    (enable-vertex-attrib-array (quad-tex-coords quad))
    (use-program program)
    (uniformi (get-uniform-location program "screenTexture") 0)
    quad))

(defmethod draw-obj ((obj quad) &optional texture)
  (when texture
    (bind-framebuffer :framebuffer 0)
    (use-program (obj-program obj))
    (bind-vertex-array (obj-vao obj))
    (bind-texture :texture-2d texture)
    (draw-elements :triangles (gl:make-null-gl-array :unsigned-int) :count 6)))

(defun scancode-is-octal-digit-p (scancode)
  (some (lambda (s)
          (sdl2:scancode= scancode s))
        '(:scancode-0 :scancode-1 :scancode-2 :scancode-3
          :scancode-4 :scancode-5 :scancode-6 :scancode-7)))

(defun minskytron ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (window :w *width* :h *height* :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl window)
        (sdl2:gl-make-current window gl)
        (opengl:viewport 0 0 *width* *height*)
        (let ((quad (make-quad))
              (minskytron (make-minskytron 64 *width* *height*)))
          (enable :blend :program-point-size)
          (blend-func :one :one-minus-src-alpha)
          (bind-framebuffer :framebuffer 0)
          (clear-color 0.0 0.0 0.0 1.0)
          (opengl:clear :color-buffer-bit)
          (clear-color 0.0 0.0 0.0 0.075)
          (sdl2:with-event-loop ()
            (:quit ()
                   (delete-framebuffers (list (minskytron-bf minskytron)))
                   t)
            (:video-expose () (sdl2:update-window window))
            (:keyup (:keysym tecla)
                    (let ((scancode (sdl2:scancode-value tecla)))
                      (cond
                        ((scancode-is-octal-digit-p scancode)
                         (minskytron-read-words minskytron scancode))
                        ((sdl2:scancode= scancode :scancode-r)
                         (minskytron-restart minskytron))
                        ((sdl2:scancode= scancode :scancode-n)
                         (setf (minskytron-data minskytron) (gen-minskytron-pars)))
                        ((sdl2:scancode= scancode :scancode-q)
                         (sdl2:push-quit-event)))))
            (:idle ()
                   (draw-obj quad (draw-obj minskytron))
                   (sdl2:gl-swap-window window))))))))
