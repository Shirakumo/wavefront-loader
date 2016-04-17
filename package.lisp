#|
 This file is a part of wavefront-loader
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:wavefront-loader
  (:nicknames #:org.shirakumo.fraf.trial.wavefront-loader)
  (:use #:cl #:3d-vectors)
  ;; geometry.lisp
  (:export
   #:draw
   #:renderable
   #:material
   #:point
   #:vertices
   #:line
   #:vertices
   #:textures
   #:face
   #:vertices
   #:textures
   #:normals
   #:mesh
   #:geometry
   #:name)
  ;; loader.lisp
  (:export
   #:load-mtl
   #:load-obj)
  ;; material.lisp
  (:export
   #:activate
   #:deactivate
   #:material
   #:name
   #:diffuse-map
   #:diffuse))
