;;;; Blog generator

;;; TODO
;;; fix date on generated page
;;; add updated date (possibly in db and published file)
;;; atom feed
;;; tag links and pages
;;; comments

;;; Generate a blog site from input posts.
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
;; (defvar *id-generator-fn* #'%id-from-file
;;   "Function used to generate a unique id for an item.")


;;; Tags used for identifying elements
(defparameter *xhtml-xmlns* "http://www.w3.org/1999/xhtml")
(defparameter *post-xmlns* "http://hugoduncan.org/xmlns/post")

(defparameter *post-when* "when")
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

(defparameter *publish-xml-indentation* 0)

;;; Singletons
(defparameter *index-page* nil)
(defparameter *atom-feed* nil)

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

(defun %id-from-file (blog-post)
  "Generate an id for the post."
  (format nil "http://~A~A" *blog-domain* *site-path* (blog-post-filename blog-post)))

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

(defmethod url-for (item)
  "Generic url-for implementation."
  (format nil "http://~A~A" *blog-domain* (path-for item)))

(defun base-url ()
  "Base url-for the blog."
  (format nil "http://~A~A" *blog-domain* *site-path*))

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
  ((filename :reader blog-post-filename)
   (title :initarg :title :accessor blog-post-title :index t)
   (tags :initarg :tags :initform nil :accessor blog-post-tags :index t)
   (when :initarg :when :initform nil :reader blog-post-when
	 :type unsigned-byte :index t
	 :documentation "When post was originally written.")
   (updated :initarg :updated :initform nil :accessor blog-post-updated
	    :type unsigned-byte :index t
	    :documentation "Last update time")
   (synopsis :initarg :synopsis :initform nil :accessor blog-post-synopsis))
  (:index t)
  (:metaclass elephant:persistent-metaclass)
  (:documentation "Metadata for blog-posts"))

(defmethod shared-initialize :after
    ((blog-post blog-post) slot-names &key &allow-other-keys)
  (setf (slot-value blog-post 'filename)
	(%sanitise-title (slot-value blog-post 'title))))

(defmethod published-file-path-for ((blog-post blog-post))
  "Find the publish file path for the specified BLOG-POST."
  (make-pathname :defaults *published-path* :name (blog-post-filename blog-post) :type "post"))

(defmethod site-file-path-for ((blog-post blog-post))
  "Find the site file path for the specified BLOG-POST."
  (merge-pathnames (make-pathname :directory '(:relative "post") :name (blog-post-filename blog-post) :type "xhtml") *site-path*))


(defmethod path-for ((blog-post blog-post))
  (format nil "~Apost/~A.xhtml" *blog-root-path* (blog-post-filename blog-post)))

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




;;;# Publishing

;;; Generate a blog post from an input post file.  The post is read and a
;;; published post file is created.  Metadata for the post is read and stored in
;;; the database.  Optionally, the site is regenerated.
(defun publish-draft (path &key (generate-site t))
  "Publish the draft post at the specified filesystem PATH. Returns a list with
the path to the published file and the site path."
  (elephant:with-open-store (*blog-db-spec*)
    (elephant:with-transaction ()
      (multiple-value-bind (output-path blog-post)
	  (%publish-draft (pathname path))
	(when generate-site
	  (generate-page blog-post))
	(list (namestring output-path) (site-file-path-for blog-post))))))

;;; Publish a draft. This puts the draft into publish, and creates database meta
;;; info for it. Returns the published file path and the blog-post metadata.
;;; If a post with the same title already exists, then we assume this is an error
;;; and raise a condition.  A restart is provided to delete the existing post and
;;; proceed.
(defun %publish-draft (path)
  "Publish the draft at the filesystem PATH."
  (multiple-value-bind (title post-when tags synopsis) (%parse-post-info path)
    (let ((existing-post (elephant:get-instances-by-value 'blog-post 'title title)))
      (if existing-post
	  (restart-case
	      (error "This blog post already exists.")
	    (delete-existing-entry ()
	      (elephant:drop-instances existing-post)))))

    (let ((draft-has-complete-metadata post-when))
      (unless post-when
	(setf post-when (decode-local-date (get-universal-time))))

      (let ((blog-post (make-instance 'blog-post :title title
				      :when (encode-date post-when)
				      :tags tags
				      :synopsis synopsis)))
	(let* ((output-path (published-file-path-for blog-post)))
	  (if draft-has-complete-metadata
	      (cl-fad:copy-file path output-path :overwrite t)
	      (%publish-draft-inserting-post-when path output-path post-when))
	  (%mark-connected-posts-dirty blog-post)
	  (values output-path blog-post))))))

;;; Parse the "title", "post-when" and "tag" elements.  Also store the first
;;; paragraph of the post to act as a synopsis.
(defun %parse-post-info (path)
  "Parse the input file at PATH, extracting the metadata.  Returns title, (day
month year), tags, and synopsis."
  (let (title tags post-when synopsis)
    (flet ((decode-when ()
	     (when post-when
	       (list (cdr (assoc :day post-when))
		     (cdr (assoc :month post-when))
		     (cdr (assoc :year post-when))))))
      (klacks:with-open-source (post (cxml:make-source path))
	(loop
	   do
	   (multiple-value-bind (key ns element) (klacks:consume post)
	     (cond
	       ((start-post-element-p key ns element *post-when*)
		(klacks:map-attributes
		 #'(lambda (ns lname qname attrib-value explicit-p)
		     (declare (ignore ns qname explicit-p))
		     (push (cons (intern (string-upcase lname) 'KEYWORD)
				 (read-from-string attrib-value)) post-when))
		 post))
	       ((start-post-element-p key ns element *post-title*)
		(setf title (klacks:consume-characters post)))
	       ((start-post-element-p key ns element *post-tag*)
		(push (klacks:consume-characters post) tags))
	       ((end-post-element-p key ns element *post-head*)
		(return nil)))))
	(klacks:find-element post *post-body*)
	(klacks:find-element post "p")
	(let* ((output (cxml:make-octet-vector-sink))
	       (tapped (klacks:make-tapping-source post output)))
	  (%find-end-element tapped "p")
	  (klacks:consume tapped)
	  (setf synopsis (sax:end-document output))))
      (values title (decode-when) tags synopsis))))

;;; When a blog post is published or changes, then some of the pages that
;;; link to the post will need to be updated.  This function finds all such
;;; pages and marks them dirty
(defun %mark-connected-posts-dirty (blog-post)
  "Mark as dirty anything that the post should cause to be regenerated"
  (setf (dirty (index-page)) t)
  (let ((recent-posts (%recent-posts)))
    (generate-page (index-page) :collection recent-posts)
    (generate-page (atom-feed) :collection recent-posts)))

;;; Publish a draft by copying it to the published path, adding the
;;; "post-when" element.  This ensures that all meta-data is in the
;;; published post, removing any reliance on maintaing the metadata
;;; in the database.
(defun %publish-draft-inserting-post-when (path output-path post-when)
  "Copy the source inserting the post-when info.  If the output file exists,
then it is overwritten (if the user has not chosen to delete an existing post,
then this code will not be executed)."
  (klacks:with-open-source (draft (cxml:make-source path))
    (with-open-file (stream output-path :direction :output
			    :element-type '(unsigned-byte 8)
			    :if-exists :supersede)
      (let* ((output (cxml:make-octet-stream-sink
		      stream :canonical nil :indentation *publish-xml-indentation*
		      :omit-xml-declaration-p t))
	     (tapped (klacks:make-tapping-source draft output)))
	(cxml:with-xml-output output
	  (klacks:find-element tapped *post-title*)
	  (klacks:find-event tapped :end-element)
	  (cxml:with-element "when"
	    (cxml:attribute "day" (first post-when))
	    (cxml:attribute "month" (second post-when))
	    (cxml:attribute "year" (nth 2 post-when)))
	  (klacks:find-event tapped :end-document))))))

;;;# Output functions
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
  (declare (ignore output blog-post))
;;   (let ((updated (blog-post-updated blog-post)))
;;     (when updated
;;       (cxml:text (format nil "~{~A~^-~}" (decode-date updated)))))
  )

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
      (cxml:attribute "href" (path-for blog-post)))
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

;;; Generates a blog page.
(defmethod generate-page ((blog-post blog-post) &key &allow-other-keys)
  "Generate the page for a blog-post.  Takes the template and merges the post into it."
  (format t "Generating post '~A'~%" (blog-post-title blog-post))
  (flet ((output-post-head-title (template)
	   "Assusmes current element is the template title"
	   (klacks:peek-next template) ; consume title tag and title text
	   (cxml:text (blog-post-title blog-post))
	   (klacks:peek-next template)))  ; close the title tag
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
	      ;; TODO - add meta links for navigation
	      (loop
		 for id = (%find-div-or-span-with-an-id tapped)
		 while id
		 for cmd = (assoc id *id-dispatch-table* :test #'string=)
		 do
		   (when cmd
		     (funcall (cdr cmd) blog-post output))))))))))


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
	    (klacks:find-event tapped :end-document)))))))

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
						  :indentation 0
						  :omit-xml-declaration-p t))
	     (tapped (klacks:make-tapping-source template output)))
	(cxml:with-xml-output output
	  (klacks:find-element tapped "updated")
	  (cxml:text (local-time:format-rfc3339-timestring nil (local-time:now)))
	  (klacks:consume tapped)
	  (loop for blog-post in collection
	     do (output-post-atom-entry blog-post output))
	  (klacks:find-event tapped :end-document))))))






;;; Hacks
(defun recent-posts ()
  (elephant:with-open-store (*blog-db-spec*)
    (elephant:with-transaction ()
      (%recent-posts))))

(defun drop-all-yes-i-know-what-i-am-doing ()
  (elephant:with-open-store (*blog-db-spec*)
    (elephant:with-transaction ()
      (elephant:drop-instances
       (elephant:get-instances-by-class 'blog-post))
      (elephant:drop-instances
       (elephant:get-instances-by-class 'generated-content)))))

;;; ensure pbook output is as intended:

;; Local Variables:
;; pbook-author:  "Hugo Duncan"
;; pbook-use-toc: t
;; pbook-style:   article
;; End:
