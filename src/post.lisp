;;;; Blog generator

;;; Generate a blog site from input posts.

;;; TODO
;;; tag links and pages
;;; comments

(in-package #:cl-blog-generator)

;;;# Configuration
;;; These are the special variables used to control the blog generator's behaviour.
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
(defvar *relative-path-fn* nil
  "The function used to generate the path of content relative to *BLOG-ROOT-PATH* or *PUBLISHED-PATH*")

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
(defparameter *element-id* "id")

(defparameter *post-content-id* "post"
  "ID of element to contain the post content")
(defparameter *post-title-id* "post-title"
  "ID of element to contain the post title")
(defparameter *post-when-id* "post-when"
  "ID of element to contain the post when date")
(defparameter *post-updated-id* "post-updated"
  "ID of element to contain the post updated date")

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

;;;## File System Helpers
;;; These functions generate local file paths based on the configuration.
(defun %template-path (key)
  "Find the path to a template for the specified KEY."
  (make-pathname :name key
		 :type (if (string= key "atom") "xml" "xhtml") :defaults *template-path*))



;;;## XHTML Helpers
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

;;; These functions find specific types of element events
(defun %find-div-with-id (template id)
  "Find the div element in the TEMPLATE with specified ID"
  (loop
     for found = nil
     for div = (klacks:find-element template "div")
     while div
     do
       (klacks:map-attributes
	#'(lambda (ns lname qname attrib-value explicit-p)
	    (declare (ignore ns qname explicit-p))
	    (when (and (string= lname *element-id*)
		       (string= attrib-value id))
	      (setf found t)))
	template)
       (klacks:consume template)
       (when found
	 (return nil))))

(defun %find-end-element (source lname)
  "Find the end element in the SOURCE with specified lname"
  (loop
     for found = nil
     for end-element = (multiple-value-list (klacks:find-event source :end-element))
     while end-element
     for (key ns name) = end-element
     do
       (when (string= lname name)
	 (return nil))))

(defun %find-div-or-span-with-an-id (template)
  "Find the next div or span element in the TEMPLATE which has an id."
  (loop
     with id = nil
     until id
     for element = (klacks:find-event template :start-element)
     while element
     do
       (klacks:map-attributes
	#'(lambda (ns lname qname attrib-value explicit-p)
	    (declare (ignore ns qname explicit-p))
	    (when (and (string= lname *element-id*))
	      (setf id attrib-value)))
	template)
       (klacks:consume template)
     finally
       (return id)))

;;; A debugging function to dump the current event
(defun %format-peek (source)
  "Dump the current event."
  (multiple-value-bind (key a b c d) (klacks:peek source)
    (format *debug-io* "~A => ~A ~A ~A ~A~%" key a b c d)))

;;;# Generated Content Protocol


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

(defgeneric link-for (generated-content)
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


;;;### Index page
(defclass index-page (generated-content)
  ((tag :initarg :tag :initform nil :reader tag-page-tag))
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

;;;### Atom feed
(defclass atom-feed (generated-content)
  ((tag :initarg :tag :initform nil :reader tag-page-tag))
  (:metaclass elephant:persistent-metaclass)
  (:documentation "Page for the main feed."))

(defmethod site-file-path-for ((atom-feed atom-feed))
  (make-pathname :name "feed.atom" :defaults *site-path*))

(defun atom-feed ()
  "Obtain the singleton atom feed."
  (or (first (elephant:get-instances-by-class 'atom-feed))
      (make-instance 'atom-feed)))


;;;### Tag pages
(defclass tag-page (generated-content)
  ((tag :initarg :tag :initform nil :reader tag-page-tag))
  (:metaclass elephant:persistent-metaclass)
  (:documentation "Page for content matching a tag."))

;;;### Blog post
(defclass blog-post (generated-content)
  ((filename :initarg :filename :reader blog-post-filename :index t)
   (title :initarg :title :accessor blog-post-title :index t)
   (tags :initarg :tags :initform nil :accessor blog-post-tags :index t)
   (when :initarg :when :initform nil :reader blog-post-when
	 :type unsigned-byte :index t
	 :documentation "When post was originally written.")
   (updated :initarg :updated :initform nil :accessor blog-post-updated
	    :type unsigned-byte :index t
	    :documentation "Last update time")
   (synopsis :initarg :synopsis :initform nil :accessor blog-post-synopsis))
  (:metaclass elephant:persistent-metaclass)
  (:documentation "Metadata for blog-posts"))

(defmethod shared-initialize :after
    ((blog-post blog-post) slot-names &key &allow-other-keys)
  (setf (slot-value blog-post 'filename)
	(if (slot-value blog-post 'filename)
	    (%sanitise-title (slot-value blog-post 'filename))
	    (%sanitise-title (slot-value blog-post 'title)))))

(defmethod relative-path-for ((blog-post blog-post))
  "Relative path for a blog post"
  (list "post" (format nil "~A" (blog-post-year blog-post))))

(defun relative-directory-for (item)
  (append (list :relative) (funcall *relative-path-fn* item)))

(defun relative-namestring-for (item)
  (format nil "~{~A/~}" (funcall *relative-path-fn* item)))

(defmethod published-file-path-for ((blog-post blog-post))
  "Find the publish file path for the specified BLOG-POST."
  (ensure-directories-exist
   (merge-pathnames
    (make-pathname :directory (relative-directory-for blog-post)
		   :name (blog-post-filename blog-post)
		   :type "post")
    *published-path*)))

(defmethod site-file-path-for ((blog-post blog-post))
  "Find the site file path for the specified BLOG-POST."
  (ensure-directories-exist
   (merge-pathnames
    (make-pathname :directory (relative-directory-for blog-post)
		   :name (blog-post-filename blog-post)
		   :type "xhtml") *site-path*)))


(defmethod path-for ((blog-post blog-post))
  (format nil "~A~A~A.xhtml" *blog-root-path*
	  (relative-namestring-for blog-post)
	  (blog-post-filename blog-post)))

(defmethod link-for ((blog-post blog-post))
  (cxml:with-element "span"
    (cxml:attribute "class" "post-link")
    (cxml:with-element "a"
      (cxml:attribute "href" (path-for blog-post))
      (cxml:text (blog-post-title blog-post)))))

(defun blog-post-year (blog-post)
  "Returns the year of the blog post."
  (destructuring-bind (day month year) (decode-date (blog-post-when blog-post))
    (declare (ignore day month))
    (values year)))

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

(defun %dirty-posts ()
  "Create a list of all dirty posts."
  (elephant:get-instances-by-value 'blog-post 'dirty t))

(defun %adjacent-posts (blog-post)
  "Return post before and after the given post."
  (assert blog-post)
  (elephant:with-btree-cursor (cursor (elephant:find-inverted-index 'blog-post 'when))
    (let (has-pair key value prior next
		   (when (blog-post-when blog-post))
		   (oid (elephant::oid blog-post)))
      (multiple-value-setq (has-pair key value)
	(elephant:cursor-set cursor when))
      (assert has-pair)
      (assert (= oid value))
      (multiple-value-setq (has-pair key value) (elephant:cursor-prev cursor))
      (if has-pair
	  (setf prior (elephant::controller-recreate-instance elephant:*store-controller* value)))
      (multiple-value-setq (has-pair key value) (elephant:cursor-next cursor))
      (multiple-value-setq (has-pair key value) (elephant:cursor-next cursor))
      (if has-pair
	  (setf next (elephant::controller-recreate-instance elephant:*store-controller* value)))
      (values prior next))))




;;;# Publishing

;;; Generate a blog post from an input post file.  The post is read and a
;;; published post file is created.  Metadata for the post is read and stored in
;;; the database.  Optionally, the site is regenerated.  The output page for
;;; the post is always generated, so it may be proofed
(defun publish-draft (path &key (generate-site nil))
  "Publish the draft post at the specified filesystem PATH. Returns a list with
the path to the published file and the site path."
  (with-open-store ()
    (multiple-value-bind (output-path blog-post)
	(%publish-draft (pathname path))
      (if generate-site
	  (generate-site)
	  (generate-page blog-post))
      (list (namestring output-path)
	    (namestring (site-file-path-for blog-post))
	    (url-for blog-post)
	    (path-for blog-post)))))

;;; Republishing uses the "updated" element in the "head" to set the updated time
;;; on the post.  If no "updated" is present, then one is added eith the current
;;; date
(defun publish-updated-post (path &key (generate-site nil))
  "Publish an updated post at the specified filesystem PATH. Returns a list with
the path to the published file and the site path."
  (with-open-store ()
    (multiple-value-bind (output-path blog-post)
	(%publish-updated-post (pathname path))
      (if generate-site
	  (generate-site)
	  (generate-page blog-post))
      (list (namestring output-path)
	    (namestring (site-file-path-for blog-post))
	    (url-for blog-post)
	    (path-for blog-post)))))

;;; Publish a draft. This puts the draft into publish, and creates database meta
;;; info for it. Returns the published file path and the blog-post metadata.
;;; If a post with the same title already exists, then we assume this is an error
;;; and raise a condition.  A restart is provided to delete the existing post and
;;; proceed.
(defun %publish-draft (path)
  "Publish the draft at the filesystem PATH."
  (multiple-value-bind (title post-when post-updated tags linkname synopsis)
      (%parse-post-info path)
    (let ((existing-post
	   (elephant:get-instances-by-value 'blog-post 'title title)))
      (if existing-post
	  (restart-case
	      (error "This blog post already exists.")
	    (delete-existing-entry ()
	      (elephant:drop-instances existing-post)))))

    (unless post-when
      (setf post-when (decode-local-date (get-universal-time))))

    (let ((blog-post
	   (make-instance 'blog-post
			  :title title
			  :when (encode-date post-when)
			  :updated (if post-updated
				       (encode-date post-updated))
			  :tags tags
			  :filename linkname
			  :synopsis synopsis)))
      (let* ((output-path (published-file-path-for blog-post)))
	(%publish-draft-updating-post-metadata path output-path blog-post)
	(%mark-connected-posts-dirty blog-post)
	(values output-path blog-post)))))



;;; Publish an updated post, adding an updated date. Returns the published file
;;; path and the blog-post metadata.
(defun %publish-updated-post (path)
  "Publish the updated post at the filesystem PATH."
  (multiple-value-bind (title post-when post-updated tags linkname synopsis)
      (%parse-post-info path)
    (declare (ignore post-when tags))
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
      (setf (blog-post-updated existing-post) (encode-date post-updated))
      (setf (blog-post-synopsis existing-post) synopsis)
      (setf (blog-post-title existing-post) title)

      (let* ((output-path (make-pathname :type "tmp" :defaults path)))
	(%publish-draft-updating-post-metadata path output-path existing-post)
	(cl-fad:copy-file output-path path :overwrite t)
	(%mark-connected-posts-dirty existing-post)
	(values output-path existing-post)))))

;;; Parse the "title", "when", "updated", "linkname" and "tag" elements.  Also store the first
;;; paragraph of the post to act as a synopsis.
(defun %parse-post-info (path)
  "Parse the input file at PATH, extracting the metadata.  Returns title,
when (day month year), updated (day month year), tags, linkname, and synopsis."
  (let (title tags post-when post-updated linkname synopsis attribs)
    (flet ((decode-date (date-data)
	     (when date-data
	       (loop for key in '(:day :month :year)
		  collect (cdr (assoc key date-data)))))
	   (attribute-mapper (ns lname qname attrib-value explicit-p)
		     (declare (ignore ns qname explicit-p))
		     (push (cons (intern (string-upcase lname) 'KEYWORD)
				 (read-from-string attrib-value)) attribs)))
      (klacks:with-open-source (post (cxml:make-source path))
	(loop
	   do
	   (multiple-value-bind (key ns element) (klacks:consume post)
	     (cond
	       ((start-post-element-p key ns element *post-when*)
		(setf attribs nil)
		(klacks:map-attributes #'attribute-mapper post)
		(setf post-when attribs))
	       ((start-post-element-p key ns element *post-updated*)
		(setf attribs nil)
		(klacks:map-attributes #'attribute-mapper post)
		(setf post-updated attribs))
	       ((start-post-element-p key ns element *post-title*)
		(setf title (klacks:consume-characters post)))
	       ((start-post-element-p key ns element *post-tag*)
		(push (klacks:consume-characters post) tags))
	       ((start-post-element-p key ns element *post-linkname*)
		(setf linkname (klacks:consume-characters post)))
	       ((end-post-element-p key ns element *post-head*)
		(return nil)))))
	(klacks:find-element post *post-body*)
	(klacks:find-element post "p")
	(let* ((output (cxml:make-octet-vector-sink))
	       (tapped (klacks:make-tapping-source post output)))
	  (%find-end-element tapped "p")
	  (klacks:consume tapped)
	  (setf synopsis (sax:end-document output))))
      (values title (decode-date post-when) (decode-date post-updated)
	      tags linkname synopsis))))

;;; When a blog post is published or changes, then some of the pages that
;;; link to the post will need to be updated.  This function finds all such
;;; pages and marks them dirty.  The index and atom-feed will need updating
;;; if the post is in the recent-posts list.
(defun %mark-connected-posts-dirty (blog-post)
  "Mark as dirty anything that the post should cause to be regenerated"
  (let ((recent-posts (%recent-posts)))
    (when (find blog-post recent-posts)
      (setf (dirty (index-page)) t)
      (setf (dirty (atom-feed)) t))
    (multiple-value-bind (prior next) (%adjacent-posts blog-post)
      (when prior
	(setf (dirty prior) t))
      (when next
	(setf (dirty next) t)))))

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
  (write-post-date (blog-post-when blog-post) *post-when*))

(defun write-post-updated (blog-post)
  (write-post-date (blog-post-updated blog-post) *post-updated*))

(defun write-post-title (blog-post)
  (let ((title (blog-post-title blog-post)))
    (when title
      (cxml:with-element *post-title*
	(cxml:text title)))))

(defun write-post-linkname (blog-post)
  (let ((linkname (blog-post-filename blog-post)))
    (when linkname
      (cxml:with-element *post-linkname*
	(cxml:text linkname)))))

(defparameter *element-dispatch-table*
  (list
    (cons *post-when*  #'write-post-when)
    (cons *post-title*  #'write-post-title)
    (cons *post-updated*  #'write-post-updated)
    (cons *post-linkname*  #'write-post-linkname)))

(defun %publish-draft-updating-post-metadata (path output-path blog-post)
  "Copy the source inserting the post-when info.  If the output file exists,
then it is overwritten (if the user has not chosen to delete an existing post,
then this code will not be executed)."
  (klacks:with-open-source (draft (cxml:make-source path))
    (with-open-file (stream output-path :direction :output
			    :element-type '(unsigned-byte 8)
			    :if-exists :supersede)
      (let* ((output (cxml:make-octet-stream-sink
		      stream :canonical nil
		      :indentation *publish-xml-indentation*
		      :omit-xml-declaration-p t))
	     (tapped (klacks:make-tapping-source draft output))
	     (elements-to-process (list *post-when* *post-updated* *post-title* *post-linkname*)))
	(labels ((write-element (name)
		   (funcall (cdr (assoc name *element-dispatch-table* :test #'string=))
			    blog-post))
		 (suppress-output-of-next-element ()
		   (setf (cxml::seen-event-p tapped) t))
		 (ensure-output-of-suppressed-element ()
		   (setf (cxml::seen-event-p tapped) nil))
		 (suppress-current-element ()
		   (loop do
			(klacks:consume tapped)
			(suppress-output-of-next-element)
		      until (eql (klacks:peek tapped) :end-element))
		   (klacks:consume tapped)))
	  (cxml:with-xml-output output
	    (loop do
		 (suppress-output-of-next-element)
		 (multiple-value-bind (key ns lname) (klacks:peek tapped)
		   (cond
		     ((null key)
		      (restart-case
			  (error "Error outputing updated post.  Please validate your editted post.")
			(return-from-output () (return nil))))
		     ((and (eql key :start-element)
			   (find lname elements-to-process :test #'string=))
		      (setf elements-to-process (delete lname elements-to-process :test #'string=))
		      (write-element lname)
		      (suppress-current-element))
		     ((and (end-post-element-p key ns lname *post-head*))
		      (loop for element in elements-to-process
			 do (write-element element))
		      (ensure-output-of-suppressed-element)
		      (klacks:consume tapped)
		      (return nil))
		     (t
		      (ensure-output-of-suppressed-element)
		      (klacks:consume tapped)))))
	    (klacks:find-event tapped :end-document)))))))

;;;# Site Generation
(defun %generate-site ()
  "Generate all dirty content for the site.  Assumes an existing database connection."
  (let ((dirty-posts (%dirty-posts))
	(index-page (index-page))
	(atom-feed (atom-feed)))
    (loop
       for blog-post in dirty-posts
       do
	 (multiple-value-bind (prior next) (%adjacent-posts blog-post)
	   (generate-page blog-post :prior prior :next next)))
    (when (or (dirty index-page) (dirty atom-feed))
      (let ((recent-posts (%recent-posts)))
	(when (dirty index-page)
	  (generate-page index-page :collection recent-posts))
	  (when (dirty atom-feed)
	    (generate-page atom-feed :collection recent-posts))))))

(defun generate-site ()
  "Generate all dirty content for the site. Creates a database connection."
  (with-open-store ()
    (%generate-site)
    (list (url-for (index-page))
	  (path-for (index-page)))))

;;;## Output functions
;;; Used to output content

(defun output-post-content (blog-post output)
  (klacks:with-open-source
      (source (cxml:make-source (published-file-path-for blog-post)))
    (loop do
	 (multiple-value-bind (key ns lname) (klacks:consume source)
	   (when (start-post-element-p key ns lname *post-body*)
	     (return nil))))
    (loop do
	 (multiple-value-bind (key ns lname) (klacks:peek source)
	   (when (end-post-element-p key ns lname *post-body*)
	     (return nil)))
       ;; serialiszing consumes the event
	 (klacks:serialize-event source output))))

(defun output-post-title (blog-post output)
  (declare (ignore output))
  (cxml:text (blog-post-title blog-post)))

(defun output-post-when (blog-post output)
  (declare (ignore output))
  (cxml:text (format nil "~{~A~^-~}" (decode-date (blog-post-when blog-post)))))

(defun output-post-updated (blog-post output)
  (declare (ignore output))
   (let ((updated (blog-post-updated blog-post)))
     (when updated
       (cxml:with-element "span"
	 (cxml:attribute "id" "post-updated-date")
	 (cxml:text (format nil "~{~A~^-~}" (decode-date updated)))))))

(defun output-post-synopsis (blog-post output)
  "Output the synopsis"
  (klacks:with-open-source
      (source (cxml:make-source (blog-post-synopsis blog-post)))
    (let ((tapped (klacks:make-tapping-source source output)))
      (klacks:find-event tapped :end-document))))

(defun output-post-atom-entry (blog-post output)
  "Output an atom entry for the post."
  (cxml:with-element "entry"
    (cxml:attribute "xml:base" (base-url))
    (cxml:with-element "title"
      (cxml:text (blog-post-title blog-post)))
    (cxml:with-element "link"
      (cxml:attribute "href" (funcall *id-generator-fn* blog-post)))
    (cxml:with-element "id" ;; TODO - fixme
      (cxml:text (url-for blog-post)))
    (cxml:with-element "published"
      (cxml:text
       (local-time:format-rfc3339-timestring
	nil (local-time:universal-to-timestamp (blog-post-when blog-post)))))
    (cxml:with-element "updated" ;; TODO - fixme
      (cxml:text
       (local-time:format-rfc3339-timestring
	nil (local-time:universal-to-timestamp (blog-post-when blog-post)))))
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
	(output-post-content blog-post output)))))

;;; Table mapping element id's to the corresponding output function
(defparameter *id-dispatch-table*
  (list
    (cons *post-content-id*  #'output-post-content)
    (cons *post-title-id*  #'output-post-title)
    (cons *post-when-id* #'output-post-when)
    (cons *post-updated-id*  #'output-post-updated)))

;;;## Page generators
;;; Generates a blog page.
(defmethod generate-page ((blog-post blog-post) &key next prior &allow-other-keys)
  "Generate the page for a blog-post.  Takes the template and merges the post into it."
  (format t "Generating post '~A'~%" (blog-post-title blog-post))
  (flet ((output-post-head-title (template)
	   "Assusmes current element is the template title"
	   (klacks:peek-next template) ; consume title tag and title text
	   (cxml:text (blog-post-title blog-post))
	   (klacks:peek-next template)) ; close the title tag
	 (output-post-link (post rel)
	   "Assusmes current element is in the document head"
	   (cxml:with-element "link"
	     (cxml:attribute "rel" rel)
	     (cxml:attribute "href" (path-for post))
	     (cxml:attribute "title" (blog-post-title post)))))
    (let ((output-path (site-file-path-for blog-post)))
      (klacks:with-open-source
	  (template (cxml:make-source (%template-path "post")))
	;; An open file error here suggests that directory components do not
	;; exist
	(with-open-file (stream output-path :direction :output
				:element-type '(unsigned-byte 8)
				:if-exists :supersede
				:if-does-not-exist :create)
	  (let* ((output (cxml:make-octet-stream-sink stream :canonical 2))
		 (tapped (klacks:make-tapping-source template output)))
	    (cxml:with-xml-output output
	      (klacks:find-element tapped *post-title*)
	      (output-post-head-title tapped)
	      (when prior
		(output-post-link prior "prev"))
	      (when next
		(output-post-link next "next"))
	      ;; TODO - add meta links for navigation
	      (loop
		 for id = (%find-div-or-span-with-an-id tapped)
		 while id
		 for cmd = (assoc id *id-dispatch-table* :test #'string=)
		 do
		   (when cmd
		     (funcall (cdr cmd) blog-post output)))))))))
  (setf (dirty blog-post) nil))


;;; Generates the index page by finding a list of recent posts, and listing
;;; links to them with a synopsis of each (the first paragraph, anyway).
(defmethod generate-page ((index-page index-page) &key collection)
  "Generate the index page for the blog."
  (format t "Generating index page~%")
  (klacks:with-open-source (template (cxml:make-source (%template-path "index")))
    (with-open-file
	(stream (site-file-path-for index-page) :direction :output
		:element-type '(unsigned-byte 8) :if-exists :supersede)
      (let* ((output (cxml:make-octet-stream-sink stream :canonical 2))
	     (tapped (klacks:make-tapping-source template output)))
	(cxml:with-xml-output output
	  (flet ((output-post-link (blog-post)
		   (link-for blog-post)
 		   (output-post-synopsis blog-post output)))
	    (%find-div-with-id tapped "posts")
	    (cxml:with-element "div"
	      (cxml:attribute "class" "post-synopsis")
	      (mapc #'output-post-link collection))
	    (klacks:find-event tapped :end-document))))))
  (setf (dirty index-page) nil))

;;; Generates the atom feed by finding a list of recent posts, and listing
;;; links to them with the full contents of each.
(defmethod generate-page ((atom-feed atom-feed) &key collection)
  "Generate the atom feed for the blog."
  (format t "Generating Atom feed~%")
  (klacks:with-open-source (template (cxml:make-source (%template-path "atom")))
    (with-open-file
	(stream (site-file-path-for atom-feed) :direction :output
		:element-type '(unsigned-byte 8) :if-exists :supersede)
      (let* ((output (cxml:make-octet-stream-sink stream :canonical nil
						  :indentation nil
						  :omit-xml-declaration-p t))
	     (tapped (klacks:make-tapping-source template output)))
	(cxml:with-xml-output output
	  (klacks:find-element tapped "updated")
	  (cxml:text (local-time:format-rfc3339-timestring nil (local-time:now)))
	  (klacks:consume tapped)
	  (klacks:consume tapped)
	  (loop for blog-post in collection
	     do (output-post-atom-entry blog-post output))
	  (klacks:find-event tapped :end-document)))))
  (setf (dirty atom-feed) nil))


;;; Defaults for configuration
(eval-when (:load-toplevel :execute)
  (setf *relative-path-fn* #'relative-path-for)
  (setf *id-generator-fn* #'url-for))


;;; Hacks
(defun recent-posts ()
  (with-open-store ()
    (%recent-posts)))

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
