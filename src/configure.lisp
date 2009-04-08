
;;;# Configuration environments

;;; Create the ability to switch between test, production, etc, environments.
;;; You are not required to use this; feel free to set the configuration
;;; variables by any means you see fit.

(in-package #:cl-blog-generator)

(defvar *environments* nil "Registered environments")

(defun set-environment (environment parameters)
  "Assign the configuration of an environment.  This does not make the
environment active."
  (let ((existing (assoc environment *environments*)))
    (if existing
	(setf (cdr existing) parameters)
	(setf *environments* (acons environment parameters *environments*))))
  (values nil))

(defun configure (environment)
  "Activate an environment."
  (flet ((symbol-for-key (key)
	   (find-symbol (format nil "*~A*" (symbol-name key))
			'#:cl-blog-generator)))
    (let ((alist-cons (assoc environment *environments*)))
      (unless alist-cons
	(error "Request for unknown configuration environment ~A" environment))
      (loop for item in (cdr alist-cons)
	 for key = (car item)
	 for symbol = (symbol-for-key key)
	 do
	 (unless symbol
	   (error "Unknown configuration key ~A" key))
	 (setf  (symbol-value symbol) (cdr item))))))

