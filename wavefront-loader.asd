#|
 This file is a part of wavefront-loader
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem wavefront-loader
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/wavefront-loader"
  :serial T
  :components ((:file "package")
               (:file "material")
               (:file "geometry")
               (:file "loader")
               (:file "documentation"))
  :depends-on (:3d-vectors
               :cl-opengl
               :cl-ppcre
               :parse-float))
