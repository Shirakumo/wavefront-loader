#|
 This file is a part of wavefront-loader
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.wavefront-loader)

(defgeneric activate (material))
(defgeneric deactivate (material))

(defun materialname (thing)
  (intern (string-upcase thing) "KEYWORD"))

(defclass material ()
  ((name :initarg :name :accessor name)
   (diffuse-map :initarg :diffuse-map :accessor diffuse-map)
   (diffuse :initarg :diffuse :accessor diffuse))
  (:default-initargs
   :name (error "NAME required.")
   :diffuse-map NIL
   :diffuse NIL))

(defmethod print-object ((material material) stream)
  (print-unreadable-object (material stream :type T)
    (format stream "~s" (name material))))

(defmethod activate ((material material))
  (when (diffuse-map material)
    (gl:bind-texture :texture-2d (diffuse-map material))
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp))
  (when (diffuse material)
    (gl:color (vx (diffuse material))
              (vy (diffuse material))
              (vz (diffuse material)))))

(defmethod deactivate ((material material))
  (when (diffuse-map material)
    (gl:bind-texture :texture-2d 0)))
