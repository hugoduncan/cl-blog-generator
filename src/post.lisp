;;;; Blog generator

;;; Generate a blog site from input posts.

;;; TODO
;;; tag links and pages
;;; comments

(in-package #:cl-blog-generator)

;;;# Configuration
;;; These are the special variables used to control the blog generator's behaviour.
(defvar *blog-title* nil
  "The blog's title.")
(defvar *blog-db-spec* nil
  "The database to be used by elephant to maintain blog metadata.")
(defvar *blog-domain* nil
  "A domain for the blog. Used to generate unique ids.")
(defvar *blog-root-path* nil
  "The path of the blog on whichever site the blog will be hosted at.") ;; should end in /
(defvar *site-path* nil
  "The local directory used to generate the site.")
(defvar *published-path* nil
  "The local directory that will contain the published posts.")
(defvar *template-path* nil
  "The local directory that contains the xhtml templates.")


;;; Optional configuration for customising the behaviour of the system
(defvar *id-generator-fn* nil
  "Function used to generate a unique id for an item.")

(defvar *blog-post-relative-path-fn* nil
  "The function used to generate the path of content relative to *BLOG-ROOT-PATH* or *PUBLISHED-PATH*")
(defvar *page-relative-path-fn* nil
  "The function used to generate the path of content relative to *BLOG-ROOT-PATH* or *PUBLISHED-PATH*")

(defvar *blog-post-path* '("post")
  "Default path list for BLOG-POST content.")
(defvar *page-path* '("page")
  "Default path list for PAGE content.")

(defvar *blog-post-template* "post"
  "Template to use for each blog-post")

(defvar *default-template* "post"
  "Template to use if none specified")

(defvar *templated-content-file-types*
  '((page . "page")
    (blog-post . "post"))
  "Default file extensions for the published content.")

(defvar *category-scheme-uri* nil
  "URI for category scheme.  If left null this will be generated to point to the tags page.")

;;; Tags used for identifying elements
(defparameter *xhtml-xmlns* "http://www.w3.org/1999/xhtml")
(defparameter *post-xmlns* "http://hugoduncan.org/xmlns/post")

(defparameter *post-when* "when")
(defparameter *post-updated* "updated")
(defparameter *post-linkname* "linkname")
(defparameter *post-tag* "tag")
(defparameter *post-title* "title")
(defparameter *post-head* "head")
(defparameter *post-body* "body")
(defparameter *post-meta* "meta")
(defparameter *post-description* "description")
(defparameter *post-name* "name")
(defparameter *post-content* "content")
(defparameter *post-template* "template")
(defparameter *page-link-for* "page-link-for")
(defparameter *element-id* "id")
(defparameter *element-class* "class")

(defparameter *feed-title* "title")
(defparameter *feed-uri* "id")
(defparameter *feed-updated* "updated")

(defparameter *post-content-id* "post"
  "ID of element to contain the post content")
(defparameter *post-title-id* "post-title"
  "ID of element to contain the post title")
(defparameter *post-when-id* "post-when"
  "ID of element to contain the post when date")
(defparameter *post-updated-id* "post-updated"
  "ID of element to contain the post updated date")
(defparameter *post-posts-id* "posts"
  "ID of element to contain the posts list")


(defparameter *publish-xml-indentation* nil)

;;;# Configuration environments

;;; Create the ability to switch between test, production, etc, environments.
;;; You are not required to use this; feel free to set the configuration
;;; variables by any means you see fit.

(defvar *environments* nil "Registered environments")

(defun set-environment (environment parameters)
  "Assign the configuration of an environment.  This does not make the environment active."
  (let ((existing (assoc environment *environments*)))
    (if existing
	(setf (cdr existing) parameters)
	(setf *environments* (acons environment parameters *environments*))))
  (values nil))

(defun configure (environment)
  "Activate an environment."
  (flet ((symbol-for-key (key)
	   (find-symbol (format nil "*~A*" (symbol-name key)) '#:cl-blog-generator)))
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


;;;# Helpers
;;; We use the post title as both the file system name of the published post,
;;; and the uri in the generated site.  To do this requires that we sanitise
;;; the name.
(defun %sanitise-title (title)
  "Sanitise a title for use as a file system path or as a http uri"
  (flet ((remove-character-p (char)
	   (find char "!.,/\\|+=`~-@#$%^&*{}[]()\"':;<>")))
    (let ((sane (remove-if #'remove-character-p (string-downcase (substitute #\_ #\Space title)))))
      (values sane))))

(defun %sanitise-synopsis (synopsis)
  "Create a description from the synopsis."
  (klacks:with-open-source
      (source (cxml:make-source synopsis :entity-resolver #'null-resolver))
    (with-output-to-string (output)
      (loop
	 for (key ns lname) = (multiple-value-list (klacks:peek-next source))
	 while key
	 do (when (eql key :characters)
	      (format output (klacks:current-characters source)))))))

;;;## File System Helpers
;;; These functions generate local file paths based on the configuration.
(defun %template-path (key)
  "Find the path to a template for the specified KEY."
  (make-pathname :name key
		 :type (if (string= key "atom") "xml" "xhtml") :defaults *template-path*))



;;;## XHTML Helpers
;;; Provide a macro for xml fragment output
(defmacro with-xml-fragment-output (sink &body body)
  "Modified with-xml-output"
  `(invoke-with-xml-fragment-output (lambda () ,@body) ,sink))

(defun invoke-with-xml-fragment-output (fn sink)
  "Modified invoke-with-xml-output"
  (let ((cxml::*sink* sink)
        (cxml::*current-element* nil)
	(cxml::*unparse-namespace-bindings* cxml::*initial-namespace-bindings*)
	(cxml::*current-namespace-bindings* nil))
    (funcall fn)))


;;; These functions provide logical tests for CXML Klacks events
(defun start-post-element-p (key ns element target)
  (and (eql key :start-element)
       (string= element target)
       (string= ns *post-xmlns*)))

(defun start-xhtml-element-p (key ns lname target)
  (and (equal key :start-element)
       (string= lname target)
       (string= ns *xhtml-xmlns*)))

(defun end-post-element-p (key ns element target)
  (and (eql key :end-element)
       (string= element target)
       (string= ns *post-xmlns*)))

;;; DTD faking resolver - Had problems with using the real DTD - it output
;;; xmlns attribues on each element.  http://paste.lisp.org/display/76827
(defun null-resolver (pubid sysid)
  "An entity resolver which does nothing."
  (declare (ignore pubid sysid))
  (flexi-streams:make-in-memory-input-stream nil))

(defun %copy-current-element-content (source sink)
  "When called with an un-output start-element, copy the content of the element.
The end-element event will be consumed but not output."
  (klacks:consume source)
  (loop with depth = 1
     for key = (klacks:peek source)
     do
       (when (and (eql key :end-element) (zerop (decf depth)))
	 (return nil))
       (when (eql key :end-element)
	 (incf depth))
       (klacks:serialize-event source sink))
  (klacks:consume source))

(defun %copy-current-element (source sink)
  "When called with an un-output start-element, copy the current element."
  (loop with depth = 0
     for key = (klacks:peek source)
     do
       (when (and (eql key :end-element) (zerop (decf depth)))
	  (return nil))
       (when (eql key :start-element)
	 (incf depth))
       (klacks:serialize-event source sink))
  (klacks:serialize-event source sink))

(defun %suppress-current-element (source)
  "When called with an un-output start-element, suppress the element."
  (klacks:consume source)
  (loop with depth = 1
     for key = (klacks:consume source)
     do (when (and (eql key :end-element) (zerop (decf depth)))
	  (return nil))
       (when (eql key :end-element)
	 (incf depth))))

(defun %skip-preamble (source)
  "Skips any XML preamble before a documents first content."
  (loop for key = (klacks:peek-next source)
     while (not (eql :start-element key))))

;;; Parse atrributes into an a-list
(defun %capture-attributes (source)
  "Capture the attributes od the element as an a-list."
  (let (attribs)
    (flet ((attribute-mapper (ns lname qname attrib-value explicit-p)
	     (declare (ignore ns qname explicit-p))
	     (push (cons (intern (string-upcase lname) 'KEYWORD)
			 (or (parse-integer attrib-value :junk-allowed t)
			     attrib-value)) attribs)))
      (klacks:map-attributes #'attribute-mapper source)
      (values attribs))))


(defun %find-end-element (source lname)
  "Find the end element in the SOURCE with specified lname"
  (loop
     for found = nil
     for (key ns name) = (multiple-value-list (klacks:find-event source :end-element))
     while key
     do
       (when (string= lname name)
	 (return nil))
       (klacks:consume source)))

(defun %copy-to-next-start-element (source sink lname)
  "Find the next LNAME element in the SOURCE outputting up to the found element
to SINK."
  (loop for (key ns ln) = (multiple-value-list (klacks:peek source))
     while key
     do
       (when (and (eql key :start-element) (string= lname ln))
	 (return (%capture-attributes source)))
       (klacks:serialize-event source sink)))


;;; A debugging function to dump the current event
(defun %format-peek (source)
  "Dump the current event."
  (multiple-value-bind (key a b c d) (klacks:peek source)
    (format *debug-io* "~A => ~A ~A ~A ~A~%" key a b c d)))

;;;# Generated Content Protocol

(defgeneric generate (page)
  (:documentation "Generate a content page"))

(defgeneric generate-page (generated-content &key collection links &allow-other-keys)
  (:documentation "Generate the requested content."))

(defgeneric published-file-path-for (generated-content)
  (:documentation "Published file path for GENERATED-CONTENT."))

(defgeneric site-file-path-for (generated-content)
  (:documentation "Site file path for the GENERATED-CONTENT."))

(defgeneric path-for (generated-content)
  (:documentation "Path for the object on the site."))

(defgeneric url-for (generated-content)
  (:documentation "Url for the object on the site."))

(defgeneric link-for (generated-content &key url)
  (:documentation "Output a link for the object."))

(defgeneric relative-path-for (generated-content)
  (:documentation "Path of an item relative to *BLOG-ROOT* or *PUBLISHED-ROOT*."))

(defmethod url-for (item)
  "Generic url-for implementation."
  (format nil "http://~A~A" *blog-domain* (path-for item)))

(defmethod relative-path-for (item)
  "Generic implementation for no relative path"
  (declare (ignore item))
  (values nil))

(defun base-url ()
  "Base url-for the blog."
  (format nil "http://~A~A" *blog-domain* *blog-root-path*))

(defun atom-category-scheme ()
  "URI for the category scheme in the ATOM feed."
  (or *category-scheme-uri*
      (format nil "http://~A~Atags" *blog-domain* *blog-root-path*)))

(defun relative-directory-for (item)
  (append (list :relative) (funcall *page-relative-path-fn* item)))

(defun relative-namestring-for (item)
  (format nil "~{~A/~}" (funcall *page-relative-path-fn* item)))


;;;# Database

;;;## Generated Content Persistent Classes
;;; The various types of pages are handled by their own class.
;;; The generated content class serves as a base class for our protocol, and enables
;;; querying for all dirty content.
(defclass generated-content ()
  ((dirty :initform t :accessor dirty :type boolean :indexed t))
  (:index t)
  (:metaclass elephant:persistent-metaclass)
  (:documentation "Base for anything that can be generated."))

;;;### Atom feed
(defclass atom-feed (generated-content)
  ((title :initarg :title :accessor atom-feed-title)
   (uri :initarg :uri :accessor atom-feed-uri))
  (:metaclass elephant:persistent-metaclass)
  (:documentation "Page for an atom feed."))

(defmethod site-file-path-for ((atom-feed atom-feed))
  (make-pathname :name "feed.atom" :defaults *site-path*))

;; (defun atom-feed ()
;;   "Obtain the singleton atom feed."
;;   (or (first (elephant:get-instances-by-class 'atom-feed))
;;       (make-instance 'atom-feed)))


;;;### Index page
(defclass index-page (generated-content)
  ((feed :initform (make-instance 'atom-feed :title *blog-title*
				  :uri (base-url)) :accessor content-feed))
  (:metaclass elephant:persistent-metaclass)
  (:documentation "Page for the main index."))

(defmethod site-file-path-for ((index-page index-page))
  (declare (ignore index-page))
  (make-pathname :name "index.xhtml" :defaults *site-path*))

(defmethod path-for ((index-page index-page))
  (format nil "~Aindex.xhtml" *blog-root-path*))

(defun index-page ()
  "Obtain the singleton index page."
  (or (first (elephant:get-instances-by-class 'index-page))
      (make-instance 'index-page)))


;;;### Tag pages
(defclass tag-page (generated-content)
  ((tag :initarg :tag :initform nil :reader tag-page-tag))
  (:metaclass elephant:persistent-metaclass)
  (:documentation "Page for content matching a tag."))

;;;### Templated content
;;; All content that uses a template to merge user written content.
(defclass templated-content (generated-content)
  ((filename :initarg :filename :reader content-filename :index t)
   (title :initarg :title :accessor content-title :index t)
   (tags :initarg :tags :initform nil :accessor content-tags :index t)
   (when :initarg :when :initform nil :reader content-when
	 :type unsigned-byte :index t
	 :documentation "When post was originally written.")
   (updated :initarg :updated :initform nil :accessor content-updated
	    :type unsigned-byte :index t
	    :documentation "Last update time")
   (description :initarg :description :initform nil :accessor content-description)
   (synopsis :initarg :synopsis :initform nil :accessor content-synopsis))
  (:metaclass elephant:persistent-metaclass)
  (:documentation "Metadata for templated user content"))

(defmethod shared-initialize :after
    ((templated-content templated-content) slot-names &key &allow-other-keys)
  (with-slots (filename title when description synopsis) templated-content
    ;; create a sanitised filename
    (setf filename
	  (if filename
	      (%sanitise-title filename)
	      (%sanitise-title title)))
    ;; default when to current day
    (unless when
      (setf when (encode-date (decode-local-date (get-universal-time)))))
    ;; create a description
    (unless (and (slot-boundp templated-content 'description) description)
      (when synopsis
	(setf description (%sanitise-synopsis synopsis))))))


(defmethod published-file-path-for ((templated-content templated-content))
  "Find the publish file path for the specified TEMPLATED-CONTENT."
  (let ((type (cdr (assoc (type-of templated-content)
			  *templated-content-file-types*))))
    (assert type)
    (ensure-directories-exist
     (merge-pathnames
      (make-pathname :directory (relative-directory-for templated-content)
		     :name (content-filename templated-content)
		     :type type)
      *published-path*))))

(defmethod site-file-path-for ((templated-content templated-content))
  "Find the site file path for the specified TEMPLATED-CONTENT."
  (ensure-directories-exist
   (merge-pathnames
    (make-pathname :directory (relative-directory-for templated-content)
		   :name (content-filename templated-content)
		   :type "xhtml") *site-path*)))


(defmethod path-for ((templated-content templated-content))
  (format nil "~A~A~A.xhtml" *blog-root-path*
	  (relative-namestring-for templated-content)
	  (content-filename templated-content)))

(defun content-year (templated-content)
  "Returns the year of the blog post."
  (destructuring-bind (day month year) (decode-date (content-when templated-content))
    (declare (ignore day month))
    (values year)))


;;;### Page
;;; A user written page that is meant to be updated over time, and is not a blog post
(defclass page (templated-content)
  ((template :initarg :template :initform nil :reader content-template))
  (:metaclass elephant:persistent-metaclass)
  (:documentation "Metadata for pages"))


(defmethod relative-path-for ((page page))
  "Relative path for PAGE."
  *page-path*)


(defmethod link-for ((page page) &key url)
  (cxml:with-element "span"
    (cxml:attribute "class" "page-link")
    (cxml:with-element "a"
      (cxml:attribute "href" (or url (path-for page)))
      (cxml:text (content-title page)))))



;;;### Blog post
;;; A blog post is content with a template of "post" which is the default template.
(defclass blog-post (templated-content)
  ((template :initform *blog-post-template* :reader content-template :allocation :class :transient t))
  (:metaclass elephant:persistent-metaclass)
  (:documentation "Metadata for blog-posts"))



(defmethod print-object ((object blog-post) stream)
  "Print a blog post instance showing date and filename"
  (print-unreadable-object (object stream :type t)
    (with-slots (filename when) object
      (format stream "(~{~A~^-~}) ~A " (decode-date when) filename))
    (format stream " oid:~D" (elephant::oid object))))

(defmethod relative-path-for ((blog-post blog-post))
  "Relative path for a blog post"
  (append *blog-post-path* (list (format nil "~A" (content-year blog-post)))))

(defmethod link-for ((blog-post blog-post) &key url)
  (cxml:with-element "span"
    (cxml:attribute "class" "post-link")
    (cxml:with-element "a"
      (cxml:attribute "href" (or url (path-for blog-post)))
      (cxml:text (content-title blog-post)))))


;;;## Dates

;;; We store dates in the database as universal time integers.  These functions
;;; provide conversion to and from an external format of (list day month year).
;;; We decode from the database using UTC, but when writing a post, we want the
;;; local time, so decode-local-date is used to obtain the local date, before
;;; being written back as UTC midnight.

(defun encode-date (dmy)
  "DMY is a list of day month year values."
  (apply #'encode-universal-time (append '(0 0 0) dmy '(0))))

(defun decode-date (utime)
  "Decode the given universal time UTC to '(day month year)"
  (subseq (multiple-value-list (decode-universal-time utime 0)) 3 6))

(defun decode-local-date (utime)
  "Decode the given universal time (in local timezone) to '(day month year)"
  (subseq (multiple-value-list (decode-universal-time utime)) 3 6))

;;;## Database Utilities
(defmacro with-open-store (() &body body)
  `(elephant:with-open-store (*blog-db-spec*)
     (elephant:with-transaction ()
       ,@body)))

(defun %recent-posts (&key (n 10))
  "Create a list of recent posts."
  (elephant:with-btree-cursor (cursor (elephant:find-inverted-index 'blog-post 'when))
    (let (has-pair key value)
      (multiple-value-setq (has-pair key value) (elephant:cursor-last cursor))
      (loop
	 for i from 0 below n
	 while has-pair
	 collect (elephant::controller-recreate-instance elephant:*store-controller* value)
	 do
	   (multiple-value-setq (has-pair key value) (elephant:cursor-prev cursor))))))

(defun %map-dirty-content (fn)
  "Map all dirty conent."
  (labels ((wrapped-fn (key value)
	     (declare (ignore key))
	     (when value
	       (funcall fn value)))
	   (fn-for-class (class)
	     (elephant:map-inverted-index #'wrapped-fn class 'dirty :value t)))
    (mapc #'fn-for-class (list 'blog-post 'page 'index-page 'atom-feed))))


(defun %adjacent-posts (blog-post)
  "Return post before and after the given post."
  (assert blog-post)
  (elephant:with-btree-cursor
      (cursor (elephant:find-inverted-index 'blog-post 'when))
    (let (has-pair key value prior next
		   (when (content-when blog-post))
		   (oid (elephant::oid blog-post)))
      (multiple-value-setq (has-pair key value)
	(elephant:cursor-set cursor when))
      (assert has-pair)
      (loop
	 while (and has-pair (not (= oid value)))
	 do (multiple-value-setq (has-pair key value) (elephant:cursor-next cursor)))
      (assert (= oid value))
      (multiple-value-setq (has-pair key value) (elephant:cursor-prev cursor))
      (if has-pair
	  (setf prior (elephant::controller-recreate-instance elephant:*store-controller* value)))
      (multiple-value-setq (has-pair key value) (elephant:cursor-next cursor))
      (assert (= oid value))
      (multiple-value-setq (has-pair key value) (elephant:cursor-next cursor))
      (if has-pair
	  (setf next (elephant::controller-recreate-instance elephant:*store-controller* value)))
      (values prior next))))




;;;# Publishing

;;; Generate a blog post from an input post file.  The post is read and a
;;; published post file is created.  Metadata for the post is read and stored in
;;; the database.  Optionally, the site is regenerated.  The output page for
;;; the post is always generated, so it may be proofed.
(defun publish-draft (path &key (generate-site nil))
  "Publish the draft post at the specified filesystem PATH. Returns a list with
the path to the published file and the site path."
  (with-open-store ()
    (multiple-value-bind (output-path templated-content)
	(%publish-draft (pathname path))
      (if generate-site
	  (generate-site)
	  (generate templated-content))
      (list (namestring output-path)
	    (namestring (site-file-path-for templated-content))
	    (url-for templated-content)
	    (path-for templated-content)))))

;;; Republishing uses the "updated" element in the "head" to set the updated time
;;; on the post.  If no "updated" is present, then one is added eith the current
;;; date.
(defun publish-updated-post (path &key (generate-site nil))
  "Publish an updated post at the specified filesystem PATH. Returns a list with
the path to the published file and the site path."
  (with-open-store ()
    (multiple-value-bind (output-path templated-content)
	(%publish-updated-post (pathname path))
      (if generate-site
	  (generate-site)
	  (generate templated-content))
      (list (namestring output-path)
	    (namestring (site-file-path-for templated-content))
	    (url-for templated-content)
	    (path-for templated-content)))))

(defun make-metadata (title when updated tags filename description
		      synopsis template)
  "Create metadata of the appropriate type"
  (unless template
    (setf template *default-template*))
  (let ((class (if (string= template *blog-post-template*) 'blog-post 'page)))
    (make-instance class
		   :title title
		   :when (if when (encode-date when))
		   :updated (if updated (encode-date updated))
		   :tags tags
		   :filename filename
		   :description description
		   :synopsis synopsis
		   :template template)))

;;; Publish a draft. This puts the draft into publish, and creates database meta
;;; info for it. Returns the published file path and the blog-post metadata.
;;; If a post with the same title already exists, then we assume this is an error
;;; and raise a condition.  A restart is provided to delete the existing post and
;;; proceed.
(defun %publish-draft (path)
  "Publish the draft at the filesystem PATH."
  (multiple-value-bind (title post-when post-updated tags linkname description
			      synopsis template)
      (%parse-post-info path)
    (let ((existing-post
	   (or
	    (elephant:get-instances-by-value 'blog-post 'title title)
	    (elephant:get-instances-by-value 'page 'title title))))
      (if existing-post
	  (restart-case
	      (error "This content already exists.")
	    (delete-existing-entry ()
	      (elephant:drop-instances existing-post)))))


    (let ((content
	   (make-metadata title post-when post-updated tags linkname
			  description synopsis template)))
      (let* ((output-path (published-file-path-for content)))
	(%publish-draft-updating-post-metadata path output-path content)
	(%mark-connected-posts-dirty content)
	(values output-path content)))))



;;; Publish an updated post, adding an updated date. Returns the published file
;;; path and the blog-post metadata.
(defun %publish-updated-post (path)
  "Publish the updated post at the filesystem PATH."
  (multiple-value-bind (title post-when post-updated tags linkname description synopsis)
      (%parse-post-info path)
    (declare (ignore post-when tags description))
    (let ((existing-post
	   (elephant:get-instances-by-value 'blog-post 'filename linkname)))
      (when existing-post
	(assert (= 1 (length existing-post)))
	(setf existing-post (first existing-post)))
      (unless existing-post
	(error "This blog post can not be found. Ensure that the linkname has not been changed."))
      (unless (equal (published-file-path-for existing-post) path)
	(error "This blog post is not in the expected location. Ensure that the file has not been moved."))

      (unless post-updated
	(setf post-updated (decode-local-date (get-universal-time))))

      ;; update metadata
      (setf (content-updated existing-post) (encode-date post-updated))
      (setf (content-synopsis existing-post) synopsis)
      (setf (content-title existing-post) title)

      (let* ((output-path (make-pathname :type "tmp" :defaults path)))
	(%publish-draft-updating-post-metadata path output-path existing-post)
	(cl-fad:copy-file output-path path :overwrite t)
	(%mark-connected-posts-dirty existing-post)
	(values output-path existing-post)))))


;;; Parse the "title", "when", "updated", "linkname" and "tag" elements.  Also store the first
;;; paragraph of the post to act as a synopsis.
(defun %parse-post-info (path)
  "Parse the input file at PATH, extracting the metadata.  Returns title,
when (day month year), updated (day month year), tags, linkname, description, and synopsis."
  (let (title tags post-when post-updated linkname description synopsis template)
    (labels ((decode-date (date-data)
	       (when date-data
		 (loop for key in '(:day :month :year)
		    collect (cdr (assoc key date-data)))))
	     )
      (klacks:with-open-source (post (cxml:make-source path :entity-resolver #'null-resolver))
	(loop
	do
	(multiple-value-bind (key ns element) (klacks:consume post)
	  (cond
	    ((start-post-element-p key ns element *post-when*)
	     (setf post-when (%capture-attributes post)))
	    ((start-post-element-p key ns element *post-updated*)
	     (setf post-updated (%capture-attributes post)))
	    ((start-post-element-p key ns element *post-meta*)
	     (let ((attribs (%capture-attributes post)))
	       (when (string= *post-description* (cdr (assoc :name attribs)))
		 (setf description (cdr (assoc :content attribs))))))
	    ((start-post-element-p key ns element *post-title*)
	     (setf title (klacks:consume-characters post)))
	    ((start-post-element-p key ns element *post-tag*)
	     (push (klacks:consume-characters post) tags))
	    ((start-post-element-p key ns element *post-linkname*)
	     (setf linkname (klacks:consume-characters post)))
	    ((start-post-element-p key ns element *post-template*)
	     (setf template (klacks:consume-characters post)))
	    ((end-post-element-p key ns element *post-head*)
	     (return nil)))))
	(klacks:find-element post *post-body*)
	(loop for key = (klacks:peek-next post)
	   while key
	   until (eql key :start-element))
	(let* ((output (cxml:make-octet-vector-sink
			:canonical nil
			:indentation nil
			:omit-xml-declaration-p t)))
	  (%copy-current-element post output)
	(setf synopsis (sax:end-document output))))
      (values title (decode-date post-when) (decode-date post-updated)
	      tags linkname description synopsis template))))

;;; When a blog post is published or changes, then some of the pages that
;;; link to the post will need to be updated.  This function finds all such
;;; pages and marks them dirty.  The index and atom-feed will need updating
;;; if the post is in the recent-posts list.
(defgeneric %mark-connected-posts-dirty (content)
  (:documentation "Mark connected objects as needing update"))

(defmethod %mark-connected-posts-dirty ((page page)))

(defmethod %mark-connected-posts-dirty ((blog-post blog-post))
  "Mark as dirty anything that the post should cause to be regenerated"
  (let ((recent-posts (%recent-posts)))
    (when (find blog-post recent-posts)
      (setf (dirty (index-page)) t))
    (multiple-value-bind (prior next) (%adjacent-posts blog-post)
      (when prior
	(setf (dirty prior) t))
      (when next
	(setf (dirty next) t)))))


(defun %mark-all-dirty ()
  "Mark everything as dirty."
  (setf (dirty (index-page)) t)
  (elephant:map-class #'(lambda (post) (setf (dirty post) t)) 'blog-post)
  (elephant:map-class #'(lambda (post) (setf (dirty post) t)) 'page))

;;; Publish a draft by copying it to the published path, adding the
;;; "post-when" element.  This ensures that all meta-data is in the
;;; published post, removing any reliance on maintaing the metadata
;;; in the database.
;;; Table mapping element id's to the corresponding output function
(defun write-post-date (date tag)
  (when date
    (let ((post-when (decode-date date)))
      (cxml:with-element tag
	(cxml:attribute "day" (first post-when))
	(cxml:attribute "month" (second post-when))
	(cxml:attribute "year" (nth 2 post-when))))))

(defun write-post-when (blog-post)
  (write-post-date (content-when blog-post) *post-when*))

(defun write-post-updated (blog-post)
  (write-post-date (content-updated blog-post) *post-updated*))

(defun write-post-title (blog-post)
  (let ((title (content-title blog-post)))
    (when title
      (cxml:with-element *post-title*
	(cxml:text title)))))

(defun write-post-linkname (blog-post)
  (let ((linkname (content-filename blog-post)))
    (when linkname
      (cxml:with-element *post-linkname*
	(cxml:text linkname)))))

(defun write-post-template (blog-post)
  (let ((template (content-template blog-post)))
    (when template
      (cxml:with-element *post-template*
	(cxml:text template)))))

(defun write-post-description (blog-post)
  (let ((description (content-description blog-post)))
    (when description
      (cxml:with-element *post-meta*
	(cxml:attribute *post-name* *post-description*)
	(cxml:attribute *post-content* description)))))

(defparameter *metadata-dispatch-table*
  (list
    (cons *post-when*  #'write-post-when)
    (cons *post-title*  #'write-post-title)
    (cons *post-updated*  #'write-post-updated)
    (cons *post-linkname*  #'write-post-linkname)
    (cons *post-template*  #'write-post-template)))


(defun %publish-draft-updating-post-metadata (path output-path blog-post)
  "Copy the source inserting the post-when info.  If the output file exists,
then it is overwritten (if the user has not chosen to delete an existing post,
then this code will not be executed)."
  (klacks:with-open-source (draft (cxml:make-source path :entity-resolver #'null-resolver))
    (with-open-file (stream output-path :direction :output
			    :element-type '(unsigned-byte 8)
			    :if-exists :supersede)
      (let* ((output (cxml:make-octet-stream-sink
		      stream :canonical nil
		      :indentation *publish-xml-indentation*
		      :omit-xml-declaration-p nil))
	     (elements-to-process (list *post-when* *post-updated* *post-title*
					*post-linkname* *post-template*))
	     (description-written-p nil))
	(labels ((write-element (name)
		   (funcall (cdr (assoc name *metadata-dispatch-table* :test #'string=))
			    blog-post)))
	  (with-xml-fragment-output output
	    (loop do
		 (multiple-value-bind (key ns lname) (klacks:peek draft)
		   (cond
		     ((null key)
		      (restart-case
			  (error "Error outputing updated post.  Please validate your editted post.")
			(return-from-output () (return nil))))
		     ((and (eql key :start-element)
			   (find lname elements-to-process :test #'string=))
		      (setf elements-to-process (delete lname elements-to-process :test #'string=))
		      (write-element lname)
		      (%suppress-current-element draft))
		     ((start-post-element-p key ns lname *post-meta*)
		      (let ((attribs (%capture-attributes draft)))
			(if (string= *post-description* (cdr (assoc :name attribs)))
			    (progn
			      (write-post-description blog-post)
			      (setf description-written-p t)
			      (%suppress-current-element draft))
			    (klacks:serialize-event draft output))))
		     ((and (end-post-element-p key ns lname *post-head*))
		      ;; output everything that hasn't already been output
		      (loop for element in elements-to-process
			 do (write-element element))
		      (unless description-written-p
			  (write-post-description blog-post))
		      (klacks:serialize-event draft output)
		      (return nil))
		     (t
		      (klacks:serialize-event draft output)))))
	    (klacks:serialize-source draft output)))))))

;;;# Site Generation
(defun %generate-site ()
  "Generate all dirty content for the site.  Assumes an existing database connection."
  (let (*recent-posts*)
    (%map-dirty-content #'generate)))

(defun generate-site (&key all)
  "Generate all dirty content for the site. Creates a database connection.  When
passed :ALL, will mark everything as dirty and regenerate (useful if you change
the templates)."
  (with-open-store ()
    (when all
      (%mark-all-dirty))
    (%generate-site)
    (list (url-for (index-page))
	  (path-for (index-page)))))

;;;## Output functions
;;; Used to output content
(defmacro with-existing-element ((template output) &body body)
  (let ((tp (gensym))
	(o (gensym))
	(k (gensym)))
    `(let ((,tp ,template)
	   (,o ,output))
       (loop for ,k = (klacks:peek ,tp)
	  while (not (eql ,k :end-element))
	  do (klacks:serialize-event ,tp ,o))
       ,@body
       (klacks:serialize-event ,tp ,o))))


(defun output-post-content-no-template (blog-post output)
  (klacks:with-open-source
      (source (cxml:make-source (published-file-path-for blog-post) :entity-resolver #'null-resolver))
    (klacks:find-element source *post-body*)
    (loop for key = (klacks:peek-next source)
       while key
       until (eql key :start-element))
    (output-with-rewrite blog-post source output)
    (loop do
	 (multiple-value-bind (key ns lname) (klacks:peek source)
	   (when (end-post-element-p key ns lname *post-body*)
	     (return nil)))
       ;; serialiszing consumes the event
	 (klacks:serialize-event source output))))

(defun output-post-content (blog-post template output attributes)
  (declare (ignore attributes))
  (with-existing-element (template output)
    (output-post-content-no-template blog-post output)))


(defun output-post-title (blog-post template output attributes)
  (declare (ignore attributes))
  (with-existing-element (template output)
    (cxml:text (content-title blog-post))))

(defun output-post-when (blog-post template output attributes)
  (declare (ignore attributes))
  (with-existing-element (template output)
    (cxml:text (format nil "~{~A~^-~}" (decode-date (content-when blog-post))))))

(defun output-post-updated (blog-post template output attributes)
  (declare (ignore attributes))
  (with-existing-element (template output)
    (let ((updated (content-updated blog-post)))
      (when updated
	(cxml:with-element "span"
	  (cxml:attribute "id" "post-updated-date")
	  (cxml:text (format nil "~{~A~^-~}" (decode-date updated))))))))

(defun output-post-synopsis (blog-post output)
  "Output the synopsis"
  (klacks:with-open-source
      (source (cxml:make-source (content-synopsis blog-post) :entity-resolver #'null-resolver))
    (%skip-preamble source)
    (output-with-rewrite blog-post source output)))



(defparameter *index-collection* nil)

(defun output-post-synopses-with-links (content template output attributes)
  (declare (ignore attributes))
  (flet ((output-post-link (blog-post)
      (cxml:with-element "div"
	(cxml:attribute "class" "post-synopsis")
	(link-for blog-post
		  :url (enough-namestring
			(site-file-path-for blog-post)
			(site-file-path-for content)))
	(output-post-synopsis blog-post output))))
    (with-existing-element (template output)
      (mapc #'output-post-link *index-collection*))))

;;; Table mapping element id's to the corresponding output function
(defparameter *id-dispatch-table*
  (list
    (cons *post-content-id*  #'output-post-content)
    (cons *post-title-id*  #'output-post-title)
    (cons *post-when-id* #'output-post-when)
    (cons *post-updated-id*  #'output-post-updated)
    (cons *post-posts-id*  #'output-post-synopses-with-links))
  "Map elements with specific id atributes to content.")


(defun output-link-for (blog-post template output attributes)
  "Output a link for the specified post or page specified in the href
attribute."
  (declare (ignore blog-post))
  (let* ((href-entry (assoc :href attributes))
	 (linkname (%sanitise-title (cdr href-entry)))
	 (linked-content
	  (some #'(lambda (class)
		    (elephant:get-instance-by-value class 'filename linkname))
		'(page blog-post))))
    (when linked-content
      (setf (cdr href-entry) (url-for linked-content))))
  (cxml:with-element "a"
    (mapc #'(lambda (x)
	      (let* ((attrib (string-downcase (string (car x))))
		     (a1 (make-array (length attrib) :element-type 'character :initial-contents attrib)))
		(cxml:attribute a1 (cdr x))))  ;; not sure why we have to copy this
	  attributes)
    (cxml:text "")
    (%copy-current-element-content template output)))


(defparameter *class-dispatch-table*
  (list
   (cons *page-link-for* #'output-link-for))
  "Map elements with specific class atributes to content.")


(defun output-feed-title (atom-feed template output attributes)
  (declare (ignore attributes))
  "Output the feed title."
  (with-existing-element (template output)
    (cxml:text (atom-feed-title atom-feed))))

(defun output-feed-uri (atom-feed template output attributes)
  (declare (ignore attributes))
  "Output the feed uri."
  (with-existing-element (template output)
    (cxml:text (atom-feed-uri atom-feed))))

(defun output-feed-updated (atom-feed template output attributes)
  (declare (ignore atom-feed attributes))
  "Output the feed updated date."
  (with-existing-element (template output)
    (cxml:text (local-time:format-rfc3339-timestring nil (local-time:now)))))

(defparameter *feed-element-dispatch-table*
  (list
   (cons *feed-title* #'output-feed-title)
   (cons *feed-uri* #'output-feed-uri)
   (cons *feed-updated* #'output-feed-updated))
  "Map specific elements to content.")

(defparameter *element-dispatch-table*
  '()
  "Map specific elements to content.")

(defmacro with-each-word ((var string) &body body)
  "Split STRING into space seperated words."
  (let ((i (gensym))
	(j (gensym))
	(s (gensym)))
    `(loop
	with ,s = ,string
	for ,i = 0 then (1+ ,j)
	for ,j = (position #\Space ,s :start ,i)
	do (let ((,var (subseq ,s ,i ,j)))
	     ,@body)
	while ,j)))

;;; The template rewriting function
(defun output-with-rewrite (templated-content source output)
  "Outputs a SOURCE, rewriting content as required to the OUTPUT sink.  Writes
only the current element."
  (flet ((dispatch (cmd attributes)
	   (when cmd
	     (funcall (cdr cmd) templated-content source output attributes)))
	 (element-cmd (lname)
	   (assoc lname *element-dispatch-table* :test #'string=))
	 (id-cmd (attributes)
	   (assoc (cdr (assoc :id attributes)) *id-dispatch-table* :test #'string=))
	 (class-cmd (attributes)
	   (with-each-word (c (cdr (assoc :class attributes)))
	     (let ((cmd (assoc c *class-dispatch-table* :test #'string=)))
	       (when cmd
		 (return cmd))))))
    (loop for key = (klacks:peek source)
       while (eql key :characters)
       do (klacks:serialize-event source output))
    (loop
       with depth = 0
       for (key ns lname) = (multiple-value-list (klacks:peek source))
       while key
       do
	 (cond
	   ((eql :start-element key)
	    (let* ((attributes (%capture-attributes source))
		   (cmd (or (element-cmd lname)
			    (id-cmd attributes)
			    (class-cmd attributes))))
	      (if cmd
		(dispatch cmd attributes)
		(progn
		 (incf depth)
		 (klacks:serialize-event source output)))))
	   ((eql :end-element key)
	    (when (minusp (decf depth))
	      (return nil))
	    (klacks:serialize-event source output))
	   (t
	    (klacks:serialize-event source output))))))

;;;## Page generators

;;; Top level generators that acquire any required resources
(defmethod generate ((page page))
  (generate-page page))

(defmethod generate ((blog-post blog-post))
  (multiple-value-bind (prior next) (%adjacent-posts blog-post)
    (generate-page blog-post :prior prior :next next)))

(defparameter *recent-posts* nil
  "Used to cache the recent-posts while generating pages")

(defmethod generate ((index-page index-page))
  (unless *recent-posts*
    (setf *recent-posts* (%recent-posts)))
  (generate-page index-page :collection *recent-posts*)
  (generate-page (content-feed index-page) :collection *recent-posts*))

(defmethod generate ((atom-feed atom-feed))
  (unless *recent-posts*
    (setf *recent-posts* (%recent-posts)))
  (generate-page atom-feed :collection *recent-posts*))



;;; Generates a page.
(defmethod generate-page ((templated-content templated-content) &key next prior &allow-other-keys)
  "Generate the page for a templated-content.  Takes the template and merges the post into it."
  (format t "Generating post '~A'~%" (content-title templated-content))
  (flet ((output-post-head-title (template output)
	   "Assusmes current element is the template title"
	   (loop for key = (klacks:peek template)
		until (eql key :end-element)
	      do (klacks:serialize-event template output)) ; consume title tag and title text
	   (cxml:text (content-title templated-content))
	   (klacks:serialize-event template output)) ; close the title tag
	 (output-post-link (post rel)
	   "Assusmes current element is in the document head"
	   (cxml:with-element "link"
	     (cxml:attribute "rel" rel)
	     (cxml:attribute "href" (path-for post))
	     (cxml:attribute "title" (content-title post))))
	 (output-post-description ()
	   "Assusmes current element is in the document head"
	   (when (content-description templated-content)
	     (cxml:with-element "meta"
	       (cxml:attribute "name" "description")
	       (cxml:attribute "content"
			       (format nil "~A"
				       (content-description templated-content))))))
	 (output-post-tags ()
	   "Assusmes current element is in the document head"
	   (when (content-tags templated-content)
	     (cxml:with-element "meta"
	       (cxml:attribute "name" "keywords")
	       (cxml:attribute "content"
			       (format nil "~{~A~^,~}"
				       (content-tags templated-content)))))))
    (let ((output-path (site-file-path-for templated-content)))
      (klacks:with-open-source
	  (template (cxml:make-source (%template-path (content-template templated-content))
				      :entity-resolver #'null-resolver))
	(with-open-file (stream output-path :direction :output
				:element-type '(unsigned-byte 8)
				:if-exists :supersede
				:if-does-not-exist :create)
	  (let ((output (cxml:make-octet-stream-sink stream :canonical nil :omit-xml-declaration-p nil)))
	    (with-xml-fragment-output output
	      (%copy-to-next-start-element template output *post-title*)
	      (output-post-head-title template output)
	      (when prior
		(output-post-link prior "prev"))
	      (when next
		(output-post-link next "next"))
	      (output-post-description)
	      (output-post-tags)
	      (%copy-to-next-start-element template output *post-body*)
	      (klacks:serialize-event template output)
	      (output-with-rewrite templated-content template output)
	      (klacks:serialize-source template output)))))))
  (setf (dirty templated-content) nil))



;;; Generates the index page by finding a list of recent posts, and listing
;;; links to them with a synopsis of each (the first paragraph, anyway).
(defmethod generate-page ((index-page index-page) &key collection)
  "Generate the index page for the blog."
  (format t "Generating index page~%")
  (klacks:with-open-source (template (cxml:make-source (%template-path "index")
						       :entity-resolver #'null-resolver))
    (with-open-file
	(stream (site-file-path-for index-page) :direction :output
		:element-type '(unsigned-byte 8) :if-exists :supersede)
      (let* ((output (cxml:make-octet-stream-sink
		      stream :canonical nil :omit-xml-declaration-p nil)))
	(with-xml-fragment-output output
	  (let ((*index-collection* collection))
	    (loop for key = (klacks:peek template)
	       while key
	       until (eql key :start-element)
	       do (klacks:serialize-event template output))
	    (output-with-rewrite index-page template output))))))
  (setf (dirty index-page) nil))


(defun output-post-atom-entry (blog-post output)
  "Output an atom entry for the post."
  (cxml:with-element "entry"
    (cxml:attribute "xml:base" (base-url))
    (cxml:with-element "title"
      (cxml:text (content-title blog-post)))
    (cxml:with-element "link"
      (cxml:attribute "href" (funcall *id-generator-fn* blog-post)))
    (cxml:with-element "id" ;; TODO - fixme
      (cxml:text (url-for blog-post)))
    (cxml:with-element "published"
      (cxml:text
       (local-time:format-rfc3339-timestring
	nil (local-time:universal-to-timestamp (content-when blog-post)))))
    (cxml:with-element "updated" ;; TODO - fixme
      (cxml:text
       (local-time:format-rfc3339-timestring
	nil (local-time:universal-to-timestamp (content-when blog-post)))))
    (cxml:with-element "published"
      (cxml:text
       (local-time:format-rfc3339-timestring
	nil (local-time:universal-to-timestamp (content-when blog-post)))))
    (loop
       with scheme = (atom-category-scheme)
       for tag in (content-tags blog-post)
       do
	 (cxml:with-element "category"
	   (cxml:attribute "scheme" scheme)
	   (cxml:attribute "term" tag)))
    (cxml:with-element "summary"
      (cxml:attribute "type" "xhtml")
      (cxml:with-element "div"
	(cxml:attribute "xmlns" *xhtml-xmlns*)
	(cxml:text "") ; force end of div start tag
	(output-post-synopsis blog-post output)))
    (cxml:with-element "content"
      (cxml:attribute "type" "xhtml")
      (cxml:with-element "div"
	(cxml:attribute "xmlns" *xhtml-xmlns*)
	(cxml:text "") ; force end of div start tag
	(output-post-content-no-template blog-post output)))))

;;; Generates the atom feed of recent posts, listing links to them with the full
;;; contents of each.
(defmethod generate-page ((atom-feed atom-feed) &key collection)
  "Generate the atom feed for the blog."
  (format t "Generating Atom feed~%")
  (klacks:with-open-source (template (cxml:make-source (%template-path "atom")
						       :entity-resolver #'null-resolver))
    (with-open-file
	(stream (site-file-path-for atom-feed) :direction :output
		:element-type '(unsigned-byte 8) :if-exists :supersede)
      (let* ((output (cxml:make-octet-stream-sink stream :canonical nil
						  :indentation nil
						  :omit-xml-declaration-p nil)))
	(with-xml-fragment-output output
	  (%copy-to-next-start-element template output "feed")
	  (klacks:serialize-event template output)
	  (let ((*element-dispatch-table* *feed-element-dispatch-table*))
	    (output-with-rewrite atom-feed template output))

;; 	  (%copy-to-next-start-element template output "updated")
;; 	  (klacks:serialize-event template output)
;; 	  (cxml:text (local-time:format-rfc3339-timestring nil (local-time:now)))
;; 	  (klacks:serialize-event template output)
	  (loop for blog-post in collection
	     do (output-post-atom-entry blog-post output))
	  (klacks:serialize-source template output)))))
  (setf (dirty atom-feed) nil))


;;; Defaults for configuration
(eval-when (:load-toplevel :execute)
  (setf *blog-post-relative-path-fn* #'relative-path-for)
  (setf *page-relative-path-fn* #'relative-path-for)
  (setf *id-generator-fn* #'url-for))

;;; Hacks
(defun drop-all-yes-i-know-what-i-am-doing ()
  (with-open-store ()
    (elephant:drop-instances
     (elephant:get-instances-by-class 'blog-post))
    (elephant:drop-instances
     (elephant:get-instances-by-class 'generated-content))))

(defun blog-index-page ()
  (site-file-path-for (index-page)))

;;; ensure pbook output is as intended:

;; Local Variables:
;; pbook-author:  "Hugo Duncan"
;; pbook-use-toc: t
;; pbook-style:   article
;; End:
