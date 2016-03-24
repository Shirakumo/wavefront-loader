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
  (name))
