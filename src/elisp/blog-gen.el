;;; -*- Mode: Emacs-Lisp -*-

;;;; Blog generator minor mode for emacs
;;;; Version 1

;;;; This code is written by Hugo Duncan and is open source.  All warranties are
;;;; disclaimed.

;;; Add this to your .emacs after adding blog-gen.el to /path/to/elisp/:
;;;
;;;   (add-to-list 'load-path "/path/to/elisp/")
;;;   (autoload 'blog-gen-mode "blog-gen"
;;;     "Minor mode for generating blog entries."
;;;     t)
;;;   (add-hook '...-mode-hook (lambda () (blog-gen-mode +1)))
;;;
;;; Usually the ... will be nxhtml.  Alternatively, you
;;; can manually toggle this mode with M-x blog-gen-mode.  Customization
;;; of blog-gen can be accomplished with `eval-after-load':
;;;
;;;   (eval-after-load 'blog-gen
;;;     '(progn ...redefine keys, &c....))
;;;

(defconst blog-gen-version 20)

(require 'slime)

;;;; Minor Mode Definition

(defvar blog-gen-mode-map (make-sparse-keymap)
  "Keymap for the blog-gen minor mode.")

(define-minor-mode blog-gen-mode
  "Minor mode for generating blog entries.
\\<blog-gen-mode-map>"
  :lighter " Blog-Gen"
  (slime-load-system "cl-blog-generator"))

(defun blog-gen-execute-and-visit (cmd-string)
  "Eval CMD-STRING in Lisp; assume output is a file name to visit."
  (slime-eval-async
   `(swank:eval-and-grab-output ,cmd-string)
   (lambda (result)
     (destructuring-bind (output value) result
       (message "%s %s" (type-of value) value)
       (let* ((res (read-from-string value))
	      (published-path (caar res))
	      (site-path (cadar res)))
	 (message "Published path is %s" published-path)
	 (message "Site path is %s" site-path)
	 (let ((buf (find-file-noselect published-path)))
	   (when buf
	     (switch-to-buffer buf)))
	 (browse-url (format "http://localhost%s" site-path))
	 (save-current-buffer
	   (set-buffer (get-buffer-create "*Site Publisher*"))
	   (insert output)
	   (when (plusp (length output))
	     (switch-to-buffer-other-window (current-buffer)))))))))

(defun blog-gen-publish-draft ()
  ;; publish a draft
  (interactive)
  (let ((cmd (format "(cl-blog-generator:publish-draft %S)" (buffer-file-name))))
    (blog-gen-execute-and-visit cmd)))


;;;; Initialization
(provide 'blog-gen)
