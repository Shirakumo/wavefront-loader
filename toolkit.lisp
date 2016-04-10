#|
 This file is a part of wavefront-loader
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.wavefront-loader)

(defun mkarray (&optional (element-type T) (initial-element NIL))
  (make-array 0 :element-type element-type :initial-element initial-element :adjustable T :fill-pointer T))

(defun insert-hash-table (table target)
  (loop for k being the hash-keys of table
        for v being the hash-values of table
        do (setf (gethash k target) v))
  target)

(defmacro with-primitives (primitive &body body)
  `(progn
     (gl:begin ,primitive)
     (unwind-protect
          (progn ,@body)
       (gl:end))))
