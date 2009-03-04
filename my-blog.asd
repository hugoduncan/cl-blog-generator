(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-blog-generator-system)
    (defpackage #:cl-blog-generator-system
      (:documentation "ASDF System package for Tiamat systems.")
      (:use :common-lisp :asdf))))

(in-package #:cl-blog-generator-system)

(defsystem #:my-blog
  :name "my-blog"
  :author "Hugo Duncan <hugo@hugoduncan.org>"
  :version "0.1"
  :maintainer "Hugo Duncan <hugo@hugoduncan.org>"
  :licence "BSD Open Source"
  :description "Configuration of blog."
  :depends-on (:cl-blog-generator)
  :components
  ((:module "src" :components ((:file "config" )))))


