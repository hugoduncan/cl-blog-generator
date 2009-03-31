;;;; Comment mail processor

;;; Process incoming comments from a mailbox

(in-package #:cl-blog-generator)

(defvar *unmoderated* nil)
(defvar *mailbox-type* nil)
(defvar *mailbox-args* nil)

;;; Top level moderate function
(defun moderate ()
  (with-open-store ()
    (process-mailbox)))

;;;# Process Mailbox
;;; Processes all incoming comments
(defun process-mailbox ()
  "Process each mail in the inbox"
  (mapc #'process-message (mel:messages (open-mailbox)))
  (values nil))

;;; Open the configured mailbox
(defun open-mailbox ()
  "Open the configured mailbox"
  (let ((fun (ecase *mailbox-type*
	       (:imap #'mel.folders.imap:make-imap-folder))))
    (apply fun *mailbox-args*)))

;;; Process a single comment message
(defun process-message (message)
  "Process a comment message"
  (restart-case
      (flet ((field (name)
	       (string-left-trim '(#\Space) (mel:field name message))))
	(let ((name (field :x-blogen-name))
	      (id (field :x-blogen-id))
	      (mail (field :x-blogen-mail))
	      (ip (field :x-blogen-ip-address))
	      (uri (field :x-blogen-uri))
	      (when (local-time:parse-rfc3339-timestring
		     (field :x-blogen-when)))
	      (text (with-output-to-string (out)
		      (with-open-stream (stream (mel:message-body-stream message))
			(loop for c = (read-char stream nil nil)
			   while c do (write-char c out))))))

	  (if id
	      (if (or *unmoderated* (moderate-message id ip name mail uri when text))
		  (add-comment id ip name mail uri (local-time:timestamp-to-universal when) text)))))
    (delete-offending-message () nil))
  (mel:delete-message message))

;;; Moderate a message
(defun moderate-message (id ip name mail uri when text)
  "Moderate a message"
  (format
   t "date: ~A~%id: ~A~%name: ~A~%mail: ~A~%uri: ~A~%ip: ~A~%Text:~%~A~%~%~%Enter with no content to post, anything to delete."
   (local-time:format-rfc3339-timestring t when) id name mail uri ip text)
  (let ((l (read-line)))
    (zerop (length l))))

