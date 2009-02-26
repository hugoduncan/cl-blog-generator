(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-blog-generator-system)
    (defpackage #:cl-blog-generator-system
      (:documentation "ASDF System package for Tiamat systems.")
      (:use :common-lisp :asdf))))

(in-package #:cl-blog-generator-system)

(defsystem #:cl-blog-generator
  :name "cl-blog-generator"
  :author "Hugo Duncan <hugo@hugoduncan.org>"
  :version "0.1"
  :maintainer "Hugo Duncan <hugo@hugoduncan.org>"
  :licence "Open source"
  :description "A generator for blog sites."
  :depends-on (:cxml :cl-fad :elephant)
  :components
  ((:module "src"
	    :components
	    ((:file "package" )
	     (:file "post" :depends-on ("package"))))))

(defsystem #:cl-blog-generator-test
  :name "cl-blog-generator-test"
  :author "Hugo Duncan <hugo@hugoduncan.org>"
  :depends-on (#:cl-blog-generator #:stefil)
  :components
  ((:module "test"
            :components
            ((:file "package")
	     (:file "post" :depends-on ("package"))))))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (find-system '#:cl-blog-generator))))
  (declare (ignore op system))
  (asdf:operate 'load-op :cl-blog-generator-test)
  (in-package #:cl-blog-generator-test)
  (declaim (optimize (debug 3)))
  (warn "(declaim (optimize (debug 3))) was issued to help later C-c C-c'ing")
  (eval (read-from-string "(stefil:funcall-test-with-feedback-message 'cl-blog-generator-test::test)"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system '#:cl-blog-generator))))
  (declare (ignore op system))
  nil)
