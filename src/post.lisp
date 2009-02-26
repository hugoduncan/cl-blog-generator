(in-package #:cl-blog-generator)

;; createdb blog

;; #!/usr/bin/env sh
;; PATH=$PATH:/opt/local/lib/postgresql83/bin
;; #db=quotes
;; db=testpg
;; #db=elepm
;; dropdb $db;
;; createdb $db;
;; psql -c "grant all on database ${db} to duncan;" $db;
;; psql -c 'create language plpgsql' $db;

(defparameter *blog-db-spec* '(:postmodern (:postgresql "127.0.0.1" "blog" "duncan" "")))
(defparameter *blog-root-path* "/blog/") ;; should end in /
(defparameter *site-path* #p"/Users/duncan/projects/blog/content/site/")
(defparameter *published-path* #p"/Users/duncan/projects/blog/content/published/")
(defparameter *template-path* #p"/Users/duncan/projects/blog/content/template/")

(defparameter *test-draft* #p"/Users/duncan/projects/blog/content/drafts/first.post")

(defparameter *xhtml-xmlns* "http://www.w3.org/1999/xhtml")
(defparameter *post-xmlns* "http://hugoduncan.org/xmlns/post")
(defparameter *post-when* "when")
(defparameter *post-tag* "tag")
(defparameter *post-title* "title")
(defparameter *post-head* "head")

(defparameter *index-page* nil)


(defun %sanitise-title (title)
  "Sanitise a title for use as a file system path or as a http uri"
  (flet ((remove-character-p (char)
	   (find char "!.,/\\|+=`~-@#$%^&*{}[]()\"':;<>")))
    (let ((sane (remove-if #'remove-character-p (string-downcase (substitute #\_ #\Space title)))))
      (values sane))))

(defun %template-path (key)
  (make-pathname :name key :type "xhtml" :defaults *template-path*))

(defun %published-file-for-draft-file (path)
  "Find the published file path for the specified input draft file."
  (merge-pathnames (make-pathname :directory '(:relative ".." "published")) (pathname (pathname path))))

(defun %published-file-for-blog-post (blog-post)
  "Find the publish file path for the specified BLOG-POST."
  (make-pathname :defaults *published-path* :name (blog-post-filename blog-post) :type "xhtml"))

(defun %site-file-for-blog-post (blog-post)
  "Find the site file path for the specified BLOG-POST."
  (merge-pathnames (make-pathname :directory '(:relative "post") :name (blog-post-filename blog-post) :type "xhtml") *site-path*))

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

(defun %format-peek (source)
  (multiple-value-bind (key ns lname qname) (klacks:peek source)
    (format t "~A => ~A ~A ~A~%" key ns lname qname)))

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
	    (when (and (string= lname "id")
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

;;; date encode/decode functions to utc times
(defun encode-date (dmy)
  "DMY is a list of day month year values."
  (apply #'encode-universal-time (append '(0 0 0) dmy '(0))))

(defun decode-date (utime)
  "Decode the given universal time UTC to '(day month year)"
  (subseq (multiple-value-list (decode-universal-time utime 0)) 3 6))

(defun decode-local-date (utime)
  "Decode the given universal time (in local timezone) to '(day month year)"
  (subseq (multiple-value-list (decode-universal-time utime)) 3 6))


(defclass generated-content ()
  ((dirty :initform t :accessor dirty :type boolean :indexed t))
  (:index t)
  (:metaclass elephant:persistent-metaclass)
  (:documentation "Base for anything that can be generated."))

(defclass index-page (generated-content)
  ((tag :initarg :tag :initform nil :reader tag-page-tag))
  (:metaclass elephant:persistent-metaclass)
  (:documentation "Page for the main index."))

(defclass tag-page (generated-content)
  ((tag :initarg :tag :initform nil :reader tag-page-tag))
  (:metaclass elephant:persistent-metaclass)
  (:documentation "Page for content matching a tag."))

(defclass blog-post (generated-content)
  ((filename :reader blog-post-filename)
   (title :initarg :title :initform nil :accessor blog-post-title :index t)
   (tags :initarg :tags :initform nil :accessor blog-post-tags :index t)
   (when :initarg :when :initform nil :accessor blog-post-when :type unsigned-byte :index t)
   (synopsis :initarg :synopsis :initform nil :accessor blog-post-synopsis))
  (:index t)
  (:metaclass elephant:persistent-metaclass)
  (:documentation "Metadata for blog-posts"))

(defmethod shared-initialize :after
    ((blog-post blog-post) slot-names &key &allow-other-keys)
  (setf (slot-value blog-post 'filename)
	(%sanitise-title (slot-value blog-post 'title))))

(defgeneric generate-page (generated-content)
  (:documentation "Generate the requested content."))

(defgeneric url-for (generated-content)
  (:documentation "Url for the object."))

(defgeneric link-for (generated-content)
  (:documentation "Output a link for the object."))


(defun index-page ()
  (unless *index-page*
    (setf *index-page* (make-instance 'index-page)))
  (values *index-page*))

(defun index-page-path ()
  (make-pathname :name "index.xhtml" :defaults *site-path*))

(defun %mark-connected-posts-dirty (blog-post)
  "Mark as dirty anything that the post should cause to be regenerated"
  (setf (dirty (index-page)) t)
  (generate-page (index-page)))

(defun %parse-post-info (path)
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
	(klacks:find-element post "body")
	(klacks:find-element post "p")
	(let* ((output (cxml:make-octet-vector-sink))
	       (tapped (klacks:make-tapping-source post output)))
	  (%find-end-element tapped "p")
	  (klacks:consume tapped)
	  (setf synopsis (sax:end-document output))))
      (values title (decode-when) tags synopsis))))

(defun %publish-draft-inserting-post-when (path output-path post-when)
  "Copy the source inserting the post-when info"
  (klacks:with-open-source (draft (cxml:make-source path))
    (with-open-file (stream output-path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
      (let* ((output (cxml:make-octet-stream-sink stream :canonical nil :indentation 2))
	     (tapped (klacks:make-tapping-source draft output)))
	(cxml:with-xml-output output
	  (klacks:find-element tapped "title")
	  (klacks:find-event tapped :end-element)
	  (cxml:with-element "when"
	    (cxml:attribute "day" (first post-when))
	    (cxml:attribute "month" (second post-when))
	    (cxml:attribute "year" (nth 2 post-when)))
	  (klacks:find-event tapped :end-document))))))

(defun %publish-draft (path)
  "Publish a draft. This puts the draft into publish, and creates database meta
info for it. Returns the published file path and the blog-post metadata."
  (multiple-value-bind (title post-when tags synopsis) (%parse-post-info path)
    (let ((inst (elephant:get-instances-by-value 'blog-post 'title title)))
      (if inst
	  (restart-case
	      (error "This blog post already exists")
	    (delete-existing-entry ()
	      (elephant:drop-instances inst)))))

    (let ((copy-mode t))
      (unless post-when
	(setf post-when (decode-local-date (get-universal-time)))
	(setf copy-mode nil))
      (let ((blog-post (make-instance 'blog-post :title title
				      :when (encode-date post-when)
				      :tags tags
				      :synopsis synopsis)))
	(%mark-connected-posts-dirty blog-post)
	(let* ((output-path (%published-file-for-blog-post blog-post)))
	  (if copy-mode
	      (cl-fad:copy-file path output-path :overwrite t)
	      (%publish-draft-inserting-post-when path output-path post-when))
	  (values output-path blog-post))))))

(defun publish-draft (path &key (generate-site t))
  "Publish a draft. This puts the draft into publish, and creates database meta
info for it. Returns the published file path."
  (elephant:with-open-store (*blog-db-spec*)
    (elephant:with-transaction ()
      (multiple-value-bind (output-path blog-post) (%publish-draft (pathname path))
	(when generate-site
	  (generate-page blog-post))
	(format nil "~A" (namestring output-path))))))


;; (defun %find-element-event (source key lname)
;;   (loop do (multiple-value-bind (k ns l) (klacks:consume ))))
(defmethod url-for ((blog-post blog-post))
  (format nil "~Apost/~A.xhtml" *blog-root-path* (blog-post-filename blog-post)))

(defmethod link-for ((blog-post blog-post))
  (cxml:with-element "span"
    (cxml:attribute "class" "post-link")
    (cxml:with-element "a"
      (cxml:attribute "href" (url-for blog-post))
      (cxml:text (blog-post-title blog-post)))))

(defmethod generate-page ((blog-post blog-post))
  "Generate the page for a blog-post.  Takes the template and merges the post into it."
  (format t "Generating post '~A'~%" (blog-post-title blog-post))
  (flet ((output-post-title (template)
	   "Assusmes current element is the template title"
	   (klacks:peek-next template) ; consume title tag an title text
	   (cxml:text (blog-post-title blog-post)))

	 (output-post-content (output)
	   (klacks:with-open-source
	       (source (cxml:make-source (%published-file-for-blog-post blog-post)))
	     (loop do
		  (multiple-value-bind (key ns lname) (klacks:consume source)
		    (when (start-post-element-p key ns lname "body")
		      (return nil))))
	     (loop do
		  (multiple-value-bind (key ns lname) (klacks:peek source)
		    (when (end-post-element-p key ns lname "body")
		      (return nil)))
		;; serialiszing consumes the event
		  (klacks:serialize-event source output)))))

    (let ((output-path (%site-file-for-blog-post blog-post)))
      (klacks:with-open-source (template (cxml:make-source (%template-path "post")))
	(with-open-file (stream output-path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
	  (let* ((output (cxml:make-octet-stream-sink stream :canonical 2))
		 (tapped (klacks:make-tapping-source template output)))
	    (cxml:with-xml-output output
	      (loop do
		   (multiple-value-bind (key ns lname) (klacks:peek tapped)
		     (cond
		       ( ;; Location for post content
			(start-xhtml-element-p key ns lname "title")
			(output-post-title tapped)
			(%find-div-with-id tapped "post")
			(output-post-content output))
		       ((eql key :end-document)
			(return (values nil)))))
		   (klacks:consume tapped)))))))))


(defun %recent-posts ()
  (elephant:with-btree-cursor (cursor (elephant:find-inverted-index 'blog-post 'when))
;;     (format t "CURSOR: ~A~%" (type-of (elephant:find-inverted-index 'blog-post 'when)))
    (let (has-pair key value)
      (multiple-value-setq (has-pair key value) (elephant:cursor-last cursor))
;;       (format t "~a ~a ~a~%" has-pair key value)
      (loop
	 for i from 0 below 10
	 while has-pair
	 collect (elephant::controller-recreate-instance elephant:*store-controller* value)
	 do
	   (multiple-value-setq (has-pair key value) (elephant:cursor-prev cursor))))))


(defun recent-posts ()
  (elephant:with-open-store (*blog-db-spec*)
    (elephant:with-transaction ()
      (%recent-posts))))

(defmethod generate-page ((index-page index-page))
  (format t "Generating index page~%")

  (klacks:with-open-source (template (cxml:make-source (%template-path "index")))
    (with-open-file (stream (index-page-path) :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
      (let* ((output (cxml:make-octet-stream-sink stream :canonical 2))
	     (tapped (klacks:make-tapping-source template output)))
	(cxml:with-xml-output output
	  (flet ((output-post-link (blog-post)
		     (link-for blog-post)
		   (klacks:with-open-source
		       (source (cxml:make-source (blog-post-synopsis blog-post)))
		     (let ((tapped (klacks:make-tapping-source source output)))
		       (klacks:find-event tapped :end-document)))))
	    (cxml:with-xml-output output
	      (%find-div-with-id tapped "posts")
	      (cxml:with-element "div"
		(cxml:attribute "class" "post-synopsis")
		(mapc #'output-post-link (%recent-posts)))
	      (klacks:find-event tapped :end-document))))))))



