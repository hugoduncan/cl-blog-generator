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

(eval-and-compile
  (defalias 'blog-gen-warn (if (fboundp 'warn) 'warn 'message)))

(require 'easymenu)


;;;; Customizations
(defgroup blog-gen nil
  "Blog generation"
  :load 'blog-gen
  :group 'applications)

(defcustom blog-gen-prefix-key "C-c C-g"
  "*Prefix key sequence for blog-gen-mode commands.
\\{blog-gen-mode-map}"
  :type  'string
  :group 'blog-gen)

(defcustom blog-gen-visit-published t
  "*Visit published pages"
  :type  'boolean
  :group 'blog-gen)

(defcustom blog-gen-browse-url nil
  "*Browse url of published pages"
  :type  'boolean
  :group 'blog-gen)

(defcustom blog-gen-browse-other-url t
  "*browse url of published pages at a different host to that generated."
  :type  'boolean
  :group 'blog-gen)

(defcustom blog-gen-other-url-prefix "http://localhost"
  "*Prefix used to construct other url."
  :type  'string
  :group 'blog-gen)

(defcustom blog-gen-package "my-blog"
  "*Package to load in CL."
  :type  'string
  :group 'blog-gen)

;;;; Minor Mode Definition
(defconst blog-gen-menu
  (let ((CONNECTEDP '(blog-gen-connected-p))
        (SLIMEP '(featurep 'slime)))
    `("Blog-Gen"
      [ "Publish draft"          blog-gen-publish-draft ,CONNECTEDP ]
      [ "Update post"            blog-gen-publish-update ,CONNECTEDP ]
      [ "Generate site"          blog-gen-generate-site ,CONNECTEDP ]
      "--"
      [ "Switch environment"      blog-gen-configure-for-environment ,CONNECTEDP ]))
  "Standard menu for the Blog-Gen minor mode.")

(defconst blog-gen-keys
  '(("p" . blog-gen-publish-draft)
    ("u" . blog-gen-publish-update)
    ("g" . blog-gen-generate-site)
    ("e" . blog-gen-configure-for-environment))
  "Standard key bindings for the Blog-Gen minor mode.")

(defvar blog-gen-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (spec blog-gen-keys)
      (let* ((key-spec (concat blog-gen-prefix-key " " (car spec)))
             (key (read-kbd-macro key-spec)))
        (define-key map key (cdr spec))))
    (easy-menu-define menu-bar-blog-gen map "Blog-Gen" blog-gen-menu)
    map)
  "Keymap for the Blog-Gen minor mode.")

(define-minor-mode blog-gen-mode
  "Minor mode for generating blog entries.
\\{blog-gen-mode-map}"
  :lighter " Blog-Gen"
  :keymap `(,(read-kbd-macro blog-gen-prefix-key) . blog-gen-mode-map)
  (blog-gen-require-cl-blog-generator)
  (when blog-gen-mode
    (easy-menu-add menu-bar-blog-gen blog-gen-mode-map)))


;;; Hooking into SLIME
(defun blog-gen-require-cl-blog-generator ()
  (if (and (featurep 'slime)
	   (slime-connected-p))
      (let ((cmd (format "(unless (cl:find-package '%s) (require '%s))"
			 blog-gen-package blog-gen-package)))
	(slime-eval-async cmd))))

(defun blog-gen-on-connect ()
  "Activate Lisp-side support for Blog-Gen."
  (slime-eval-async (list 'cl:require blog-gen-package)))

(defun blog-gen-slime-install ()
  "Install Blog-Gen hook for SLIME connections."
  (add-hook 'slime-connected-hook 'blog-gen-on-connect))

(defun blog-gen-slime-uninstall ()
  "Uninstall Blog-Gen hook from SLIME."
  (remove-hook 'slime-connected-hook 'blog-gen-on-connect))

;;;; Utility Functions
(defun blog-gen-connected-p ()
  "Checks whether Blog-Gen is connected to an inferior Lisp via SLIME."
  (and (featurep 'slime)
       (slime-connected-p)
       (slime-eval `(cl:packagep (cl:find-package :cl-blog-generator)))))



(defun blog-gen-output-buffer ()
  "FInd or create the output buffer"
  (let ((output-buffer (get-buffer "*Site Publisher*")))
    (unless output-buffer
      (setf output-buffer (generate-new-buffer "*Site Publisher*"))
      (with-current-buffer output-buffer
	(compilation-minor-mode)))
    output-buffer))

(defun blog-gen-execute-and-visit (cmd-string)
  "Eval CMD-STRING in Lisp; assume output is a file name to visit."
  (slime-eval-async
   `(swank:eval-and-grab-output ,cmd-string)
   (lambda (result)
     (destructuring-bind (output value) result
       (when value
	 (let* ((res (read-from-string value))
		(published-path (nth 0 res))
		(site-path (nth 1 res))
		(url (nth 2 res))
		(path (nth 3 res)))
	   (message "Published path is %s" published-path)
	   (message "Site path is %s" site-path)
	   (message "Url is %s" url)
	   (message "Path is %s" path)
	   (when blog-gen-visit-published
	     (let ((buf (find-file-noselect published-path)))
	       (when buf
		 (switch-to-buffer buf))))
	   (when blog-gen-browse-other-url
	     (browse-url (format "%s%s" blog-gen-other-url-prefix path)))
	   (when blog-gen-browse-url
	     (browse-url (format "%s" url)))
	   (with-current-buffer (blog-gen-output-buffer)
	     (when (plusp (length output))
	       (insert output)
	       (switch-to-buffer-other-window (current-buffer))))))))))

;;; Interactive functions
(defun blog-gen-publish-draft ()
  ;; publish a draft
  (interactive)
  (let ((cmd (format "(cl-blog-generator:publish-draft %S)" (buffer-file-name))))
    (blog-gen-execute-and-visit cmd)))

(defun blog-gen-publish-update ()
  ;; update a published post
  (interactive)
  (let ((cmd (format "(cl-blog-generator:publish-updated-post %S)" (buffer-file-name))))
    (blog-gen-execute-and-visit cmd)))

(defun blog-gen-generate-site ()
  ;; Generate the site content
  (interactive)
  (let ((cmd (format "(cl-blog-generator:generate-site)")))
    (slime-eval-async
     `(swank:eval-and-grab-output ,cmd)
     (lambda (result)
       (destructuring-bind (output value) result
	 (when value
	   (let* ((url (nth 0 res))
		  (path (nth 1 res)))
	   (message "Url is %s" url)
	   (message "Path is %s" path)
	   (when blog-gen-browse-other-url
	     (browse-url (format "%s%s" blog-gen-other-url-prefix path)))
	   (when blog-gen-browse-url
	     (browse-url (format "%s" url)))
	   (with-current-buffer (blog-gen-output-buffer)
	     (when (plusp (length output))
	       (insert output)
	       (switch-to-buffer-other-window (current-buffer)))))))))))

(defun blog-gen-configure-for-environment (env)
  ;; Change the blog environment
  (interactive "SEnvironment :")
  (let ((cmd (format "(cl-blog-generator:configure :%S)" env)))
    (slime-eval-async
     `(swank:eval-and-grab-output ,cmd))))


;;;; Initialization
(eval-after-load "slime"
  '(progn
    (blog-gen-slime-install)
    (when (slime-connected-p)
      (blog-gen-on-connect))))

(provide 'blog-gen)
