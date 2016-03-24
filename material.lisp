#|
 This file is a part of wavefront-loader
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.wavefront-loader)

(defgeneric activate (material))

(defun materialname (thing)
  (intern (string-upcase thing) "KEYWORD"))

(defclass material ()
  ((name :initarg :name :accessor name)
   (diffuse :initarg :diffuse :accessor diffuse))
  (:default-initargs
   :name (error "NAME required.")))

(defmethod print-object ((material material) stream)
  (print-unreadable-object (material stream :type T)
    (format stream "~s" (name material))))

(defmethod activate ((material material))
  (when (diffuse material)
    (gl:bind-texture :texture-2d (diffuse material))
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp)))
