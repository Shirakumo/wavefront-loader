#|
 This file is a part of wavefront-loader
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem wavefront-loader
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://Shirakumo.github.io/wavefront-loader/"
  :bug-tracker "https://github.com/Shirakumo/wavefront-loader/issues"
  :source-control (:git "https://github.com/Shirakumo/wavefront-loader.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "material")
               (:file "geometry")
               (:file "loader")
               (:file "documentation"))
  :depends-on (:3d-vectors
               :cl-opengl
               :cl-ppcre
               :parse-float))
