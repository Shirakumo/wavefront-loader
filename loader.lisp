#|
 This file is a part of wavefront-loader
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.wavefront-loader)

(defun skip-line (in)
  (loop for char = (read-char in NIL NIL)
        while char
        until (or (char= char #\Linefeed)
                  (and (char= char #\Return)
                       (not (char= char #\Linefeed (peek-char NIL in)))))))

(defun read-wavefront-line (in)
  (when (peek-char T in NIL NIL)
    (string-trim '(#\Space #\Tab)
                 (with-output-to-string (out)
                   (loop for char = (read-char in NIL NIL)
                         while char
                         do (case char
                              ((#\Linefeed #\Return)
                               (return))
                              (#\\
                               (let ((char (read-char in)))
                                 (case char
                                   (#\Linefeed)
                                   (#\Return ; Special case handling for CRLF skipping
                                    (when (char= (peek-char NIL in) #\Linefeed)
                                      (read-char in)))
                                   (T
                                    (write-char char out)))))
                              (#\#
                               (skip-line in)
                               (return))
                              (T
                               (write-char char out))))))))

(defmacro with-processing-case (line &body commands)
  (let ((l (gensym "LINE")))
    `(let ((,l ,line))
       (or ,@(loop for (command . body) in commands
                   for (regex . args) = (if (listp command) command (list command))
                   collect `(cl-ppcre:register-groups-bind ,args (,(format NIL "^~a$" regex) ,l)
                              ,@body
                              T))
           (warn "Unparseable line ~s" ,l)))))

(defun parse-vec (x y z)
  (vec (if x (parse-float:parse-float x) 0.0)
       (if y (parse-float:parse-float y) 0.0)
       (if z (parse-float:parse-float z) 0.0)))

(defun parse-ref (refspec)
  (destructuring-bind (vertex &optional texture normal) (cl-ppcre:split "/" refspec)
    (list (parse-integer vertex)
          (when (and texture (string/= texture "")) (parse-integer texture))
          (when (and normal (string/= normal "")) (parse-integer normal)))))

(defgeneric load-mtl (source)
  (:method ((pathname pathname))
    (with-open-file (stream pathname :direction :input
                                     :if-does-not-exist :error
                                     :element-type 'character)
      (load-mtl stream)))
  (:method ((string string))
    (with-input-from-string (stream string)
      (load-mtl stream))))

(defmethod load-mtl ((stream stream))
  (let ((materials (make-hash-table :test 'eql))
        (material NIL))
    (loop for line = (read-wavefront-line stream)
          while line
          do (with-processing-case line
               ("")
               (("newmtl +([^ ]+)" name)
                (let ((name (materialname name)))
                  (setf material (make-instance 'material :name name))
                  (setf (gethash name materials) material)))
               (("Kd +(-?[0-9.]+) (-?[0-9.]+) (-?[0-9.]+)" x y z)
                (setf (diffuse material) (parse-vec x y z)))
               (("map_Kd +(.*)" file)
                (setf (diffuse-map material) (merge-pathnames file (pathname stream))))))

    materials))

(defgeneric load-obj (source)
  (:method ((pathname pathname))
    (with-open-file (stream pathname :direction :input
                                     :if-does-not-exist :error
                                     :element-type 'character)
      (load-obj stream)))
  (:method ((string string))
    (with-input-from-string (stream string)
      (load-obj stream))))

(defmethod load-obj ((stream stream))
  (let ((vertices (mkarray '(or null vec)))
        (textures (mkarray '(or null vec)))
        (normals (mkarray '(or null vec)))
        (materials (make-hash-table :test 'eql))
        (material NIL)
        (meshes (mkarray '(or null mesh))))
    (labels ((add (element target)
               (vector-push-extend element target))
             (resolve-single (num from)
               (when num
                 (if (< num 0)
                     (elt from (+ (length from) num))
                     (elt from (1- num)))))
             (resolve-ref (nums)
               (destructuring-bind (vertex texture normal) nums
                 (list (resolve-single vertex vertices)
                       (resolve-single texture textures)
                       (resolve-single normal normals))))
             (map-refs (refspecs function)
               (loop for refspec in (remove "" (cl-ppcre:split " +" refspecs) :test #'string=)
                     do (apply function (resolve-ref (parse-ref refspec)))))
             (add-mesh (&optional name)
               (add (make-instance 'mesh :name name :material material) meshes))
             (mesh ()
               (when (= 0 (length meshes)) (add-mesh))
               (elt meshes (1- (length meshes)))))
      (loop for line = (read-wavefront-line stream)
            while line
            do (with-processing-case line
                 ("")
                 (("mtllib +(.*)" files)
                  ;; Need to load in reverse so that earlier files override later ones.
                  (loop for file in (reverse (cl-ppcre:split " +" files))
                        do (insert-hash-table (load-mtl (merge-pathnames file (pathname stream)))
                                              materials)))
                 (("usemtl +([^ ]+)" name)
                  (setf material (or (gethash (materialname name) materials)
                                     (error "No such material ~s!" material)))
                  (unless (= 0 (length meshes))
                    (setf (material (mesh)) material)))
                 (("v +(-?[0-9.]+) (-?[0-9.]+) (-?[0-9.]+)" x y z)
                  (add (parse-vec x y z) vertices))
                 (("vn +(-?[0-9.]+) (-?[0-9.]+) (-?[0-9.]+)" x y z)
                  (add (parse-vec x y z) normals))
                 (("vt +(-?[0-9.]+)( (-?[0-9.]+)( (-?[0-9.]+))?)?" x NIL y NIL z)
                  (add (parse-vec x y z) textures))
                 (("p(( +-?[0-9]+)+)" refspecs NIL)
                  (let ((point (make-instance 'point)))
                    (map-refs refspecs (lambda (vertex texture normal)
                                         (declare (ignore texture normal))
                                         (add vertex (vertices point))))
                    (add point (geometry (mesh)))))
                 (("l(( +-?[0-9]+(/(-?[0-9]+)?)?)+)" refspecs)
                  (let ((line (make-instance 'line)))
                    (map-refs refspecs (lambda (vertex texture normal)
                                         (declare (ignore normal))
                                         (add vertex (vertices line))
                                         (add texture (textures line))))
                    (add line (geometry (mesh)))))
                 (("f(( +-?[0-9]+(/(-?[0-9]+)?(/(-?[0-9]+)?)?)?)+)" refspecs)
                  (let ((face (make-instance 'face)))
                    (map-refs refspecs (lambda (vertex texture normal)
                                         (add vertex (vertices face))
                                         (add texture (textures face))
                                         (add normal (normals face))))
                    (add face (geometry (mesh)))))
                 (("o +(.*)" name)
                  (add-mesh name))
                 (("g +(.*)" name)
                  (add-mesh name)))))
    meshes))
