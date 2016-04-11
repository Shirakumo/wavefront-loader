#|
 This file is a part of wavefront-loader
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.wavefront-loader)

(defgeneric draw (renderable))

(defclass renderable ()
  ((material :initarg :material :accessor material))
  (:default-initargs
   :material NIL))

(defmethod draw :around ((renderable renderable))
  (cond ((material renderable)
         (activate (material renderable))
         (call-next-method)
         (deactivate (material renderable)))
        (T
         (call-next-method))))

(defclass point (renderable)
  ((vertices :initarg :vertices :accessor vertices))
  (:default-initargs
   :vertices (mkarray '(or null vec))))

(defmethod draw ((point point))
  (with-primitives :points
    (loop for vertex across (vertices point)
          do (gl:vertex (vx vertex) (vy vertex) (vz vertex)))))

(defclass line (renderable)
  ((vertices :initarg :vertices :accessor vertices)
   (textures :initarg :textures :accessor textures))
  (:default-initargs
   :vertices (mkarray '(or null vec))
   :textures (mkarray '(or null vec))))

(defmethod draw ((line line))
  (with-primitives :lines
    (loop for texture across (textures line)
          for vertex across (vertices line)
          do (gl:tex-coord (vx texture) (vy texture) (vz texture))
             (gl:vertex (vx vertex) (vy vertex) (vz vertex)))))

(defclass face (renderable)
  ((vertices :initarg :vertices :accessor vertices)
   (textures :initarg :textures :accessor textures)
   (normals :initarg :normals :accessor normals))
  (:default-initargs
   :vertices (mkarray '(or null vec))
   :textures (mkarray '(or null vec))
   :normals (mkarray '(or null vec))))

(defmethod draw ((face face))
  (with-primitives (case (length (vertices face))
                        (1 :points)
                        (2 :lines)
                        (3 :triangles)
                        (4 :quads)
                        (5 :triangle-strip))
    (loop for texture across (textures face)
          for normal across (normals face)
          for vertex across (vertices face)
          do (gl:tex-coord (vx texture) (vy texture) (vz texture))
             (gl:normal (vx normal) (vy normal) (vz normal))
             (gl:vertex (vx vertex) (vy vertex) (vz vertex)))))

(defclass mesh (renderable)
  ((geometry :initarg :geometry :accessor geometry)
   (name :initarg :name :accessor name))
  (:default-initargs
   :geometry (mkarray '(or null renderable))
   :name NIL))

(defmethod print-object ((mesh mesh) stream)
  (print-unreadable-object (mesh stream :type T :identity T)
    (format stream "~@[~a~]" (name mesh))))

(defmethod draw ((mesh mesh))
  (loop for geom across (geometry mesh)
        do (draw geom)))
