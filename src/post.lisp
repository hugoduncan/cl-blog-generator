;;;; Blog generator

;;; Generate a blog site from input posts.

;;; TODO
;;; Comment feed
;;; Documentation
;;; Use gravatar in comments
;;; Index page for tags
;;; Twitter comments

(in-package #:cl-blog-generator)

;;;# Configuration

;;; These are the special variables used to control the blog generator's
;;; behaviour.

;;; The blog's title.
(defvar *blog-title* nil
  "The blog's title.")

;;;## Paths
;;; These determine the URL of the blog on its website.
(defvar *blog-domain* nil
  "A domain for the blog. Used to generate unique ids.")
(defvar *blog-root-path* nil
  "The path of the blog root on the hosted web site.") ; ends in /

;;; Local file system paths for content.
(defvar *site-path* nil
  "The local directory in which to generate the site.")
(defvar *published-path* nil
  "The local directory that contains the published posts.")
(defvar *template-path* nil
  "The local directory that contains the XHTML templates.")

;;; These determine the path components on the generated web site under
;;; *BLOG-ROOT-PATH*, and on the local file system under the *PUBLISHED-PATH*
;;; and *SITE-PATH*.  These are used by the default path functions.
(defvar *blog-post-path* '("post")
  "Default path list for BLOG-POST content.")
(defvar *page-path* '("page")
  "Default path list for PAGE content.")
(defvar *tag-page-path* '("tag")
  "Default path list for TAG-PAGE content.")
(defvar *comment-path* '("comment")
  "Default path list for COMMENT, relative to *PUBLISHED-PATH*")

;;; These are customisation points for the content paths, and only need to be
;;; altered if you want a path structure different to the default.
(defvar *blog-post-relative-path-fn* nil
  "The function used to generate the path of content relative to
  *BLOG-ROOT-PATH* or *PUBLISHED-PATH*")
(defvar *page-relative-path-fn* nil
  "The function used to generate the path of content relative to
  *BLOG-ROOT-PATH* or *PUBLISHED-PATH*")

;;; Default templates to use when none are specified.
(defvar *blog-post-template* "post"
  "Template to use for each blog-post")
(defvar *default-template* "post"
  "Template to use if none specified")

;;; File types (extensions) to use when generating published file names.
(defvar *templated-content-file-types*
  '((page . "page")
    (blog-post . "post")
    (comment . "comment"))
  "Default file extensions for the published content.")

;;;## Atom

;;; ATOM requires unique id's for each feed element.  The default is to use the
;;; post's URL. This function can be set to overide the default.
(defvar *id-generator-fn* nil
  "Function used to generate a unique id for an item.")

;;; The post tags appear in the generatom ATOM feed.  The category scheme can be
;;; customised here.
(defvar *category-scheme-uri* nil
  "URI for category scheme.  If null, the tag page URL is used.")

;;;## Other

;;; The indentation in the generated files can be controlled.  See CXML
;;; documentation for values.
(defparameter *publish-xml-indentation* nil
  "Controls the indentation in the generated files.")



;;;# Template Id's and Class's
(defparameter *post-content-id* "post"
  "ID of element to contain the post content")
(defparameter *post-title-id* "post-title"
  "ID of element to contain the post title")
(defparameter *post-when-id* "post-when"
  "ID of element to contain the post when date")
(defparameter *post-updated-id* "post-updated"
  "ID of element to contain the post updated date")

(defparameter *post-posts-class* "posts"
  "Class of element to contain the posts list")
(defparameter *post-synopsis-class* "post-synopsis"
  "Class of element to contain a post's synopsis")
(defparameter *post-link-class* "post-link"
  "Class of element to contain a post's link")
(defparameter *post-tags-class* "post-tags"
  "Class of element to contain the post's tags")
(defparameter *post-tag-class* "post-tag"
  "Class of element to contain a post tags")

(defparameter *tag-name-class* "tag-name"
  "Class of element to contain the tag's name")
(defparameter *tag-related-class* "tags-related"
  "Class of element to contain the tag's related tags")

(defparameter *comment-count-class* "comment-count"
  "Class of element to contain the count of the comments")
(defparameter *comments-class* "comments-list"
  "Class of element to contain the comments")
(defparameter *comment-class* "comment-entry"
  "Class of element to contain a comment")

(defparameter *comment-id-class* "comment-id"
  "Class of element to contain a comment id")
(defparameter *comment-link-class* "comment-link"
  "Class of element to contain a comment's link")
(defparameter *comment-from-class* "comment-from"
  "Class of element to contain a comment's originator")
(defparameter *comment-when-class* "comment-when"
  "Class of element to contain a comment's timestamp")

(defparameter *post-slug-hidden-input-class* "post-slug-hidden-input"
  "Class of element to contain a hidden input element containing the page slug")

(defparameter *delicious-class* "delicious"
  "Class for posting to delicious")
(defparameter *digg-class* "digg"
  "Class for posting to digg")

(defparameter *page-link-for-class* "page-link-for")

;;;# Tags used for identifying elements
;;; These should be constants...
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
(defparameter *post-link* "link")
(defparameter *post-description* "description")
(defparameter *post-name* "name")
(defparameter *post-content* "content")
(defparameter *post-template* "template")
(defparameter *element-id* "id")
(defparameter *element-class* "class")
(defparameter *rel-alternate* "alternate")
(defparameter *href* "href")

(defparameter *comment-ip* "ip")
(defparameter *comment-name* "name")
(defparameter *comment-email* "email")
(defparameter *comment-uri* "uri")
(defparameter *comment-when* "when")
(defparameter *comment-text* "text")

(defparameter *atom-mime* "application/atom+xml")
(defparameter *feed-title* "title")
(defparameter *feed-uri* "id")
(defparameter *feed-updated* "updated")


;;;# Helpers
;;; We use the post title as both the file system name of the published post
;;; and the uri in the generated site.  To do this requires that we sanitise
;;; the name.
(defun %sanitise-title (title)
  "Sanitise a title for use as a file system path or as a http uri"
  (flet ((remove-character-p (char)
           (find char "!.,/\\|+=`~-@#$%^&*{}[]()\"':;<>")))
    (let ((sane (remove-if
                 #'remove-character-p
                 (string-trim
                  '(#\Space #\Tab #\Newline)
                  (string-downcase (substitute #\_ #\Space title))))))
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

(defun merge-assoc (a b &key (test #'eql))
  "Merge association lists A and B, such that values from B take precedence."
  (loop with result = (copy-seq a)
     for kv in b
     for assoc = (assoc (car kv) a :test test)
     do
       (if assoc
           (setf (cdr assoc) (cdr kv))
           (setf result (acons (car kv) (cdr kv) result)))
     finally
       (return result)))

;;;## File System Helpers
;;; These functions generate local file paths based on the configuration.
(defun %template-path (key)
  "Find the path to a template for the specified KEY."
  (make-pathname :name key
                 :type (if (string= key "atom") "xml" "xhtml")
                 :defaults *template-path*))



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

(defun start-comment-element-p (key ns element target)
  (declare (ignore ns))
  (and (eql key :start-element)
       (string= element target)))

(defun end-comment-element-p (key ns element target)
  (declare (ignore ns))
  (and (eql key :end-element)
       (string= element target)))

;;; DTD faking resolver - Had problems with using the real DTD - it output
;;; xmlns attribues on each element.  http://paste.lisp.org/display/76827
(defun null-resolver (pubid sysid)
  "An entity resolver which does nothing."
  (declare (ignore pubid sysid))
  (flexi-streams:make-in-memory-input-stream nil))

(defun %copy-current-element-content (source sink)
  "When called with an un-output start-element, copy the content of the element.
The end-element event will be consumed but not output."
;;   (klacks:consume source)
  (loop with depth = 1
     for key = (klacks:peek source)
     do
       (when (and (eql key :end-element) (zerop (decf depth)))
         (return nil))
       (when (eql key :start-element)
         (incf depth))
       (klacks:serialize-event source sink))
;;   (klacks:consume source)
  )

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
             (push (cons lname attrib-value) attribs)))
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
to SINK. Returns the attributes of the element found."
  (loop for (key ns ln) = (multiple-value-list (klacks:peek source))
     while key
     do
       (when (and (eql key :start-element) (string= lname ln))
         (return (%capture-attributes source)))
       (klacks:serialize-event source sink)))

(defun %skip-to-next-start-element (source lname)
  "Find the next LNAME element in the SOURCE."
  (loop for (key ns ln) = (multiple-value-list (klacks:peek source))
     while key
     do
       (when (and (eql key :start-element) (string= lname ln))
         (return key))
       (klacks:consume source)))


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
(defvar *blog* nil)

(defun %blog-add (where instance)
  (push instance (gethash where (%blog)))
  (when (find where '(:posts :pages :index-page))
    (setf (gethash where *blog*)
          (sort (gethash where *blog*) '> :key 'content-when)))
  (when (equal where :tag-pages)
    (setf (gethash where *blog*)
          (sort (gethash where *blog*) 'string> :key 'tag-page-tag))))

;;;## Generated Content Persistent Classes
;;; The various types of pages are handled by their own class.  The generated
;;; content class serves as a base class for our protocol, and enables querying
;;; for all dirty content.
(defclass generated-content ()
  ((dirty :initform t :accessor dirty :type boolean))
  (:documentation "Base for anything that can be generated."))

(defmethod shared-initialize :after
    ((generated-content generated-content) slot-names &key &allow-other-keys)
  (%blog-add :all generated-content))

;;;### Atom feed
(defclass atom-feed ()
  ((title :initarg :title :accessor atom-feed-title)
   (uri :initarg :uri :accessor atom-feed-uri)
   (content-page :initarg :content-page :reader content-page))
  (:documentation "Page for an atom feed."))

(defmethod site-file-path-for ((atom-feed atom-feed))
  (make-pathname :type "atom"
                 :defaults (site-file-path-for (content-page atom-feed))))

(defmethod path-for ((atom-feed atom-feed))
  (make-pathname :type "atom" :defaults (path-for (content-page atom-feed))))


;;;### Index page
(defclass index-page (generated-content)
  ((feed :accessor content-feed))
  (:documentation "Page for the main index."))

(defmethod site-file-path-for ((index-page index-page))
  (declare (ignore index-page))
  (make-pathname :name "index" :type "xhtml" :defaults *site-path*))

(defmethod path-for ((index-page index-page))
  (format nil "~Aindex.xhtml" *blog-root-path*))

(defvar *index-page* nil)
(defun index-page ()
  "Obtain the singleton index page."
  (or *index-page*
      (setf *index-page* (make-instance 'index-page))))

(defmethod shared-initialize :after
    ((index-page index-page) slot-names &key &allow-other-keys)
  (with-slots (feed) index-page
    (setf feed
          (make-instance 'atom-feed
                         :title *blog-title*
                         :uri (base-url)
                         :content-page index-page)))
  (%blog-add :index-page index-page))

(defmethod content-title ((index-page index-page))
  "The blog title is returned as empty in the expectation that it is specified
in the template"
  "")

;;;### Tag pages
(defclass tag-page (generated-content)
  ((filename :initarg :filename :reader content-filename)
   (tag :initarg :tag :initform nil :reader tag-page-tag)
   (related-tags :initarg :related-tags :initform nil
                 :accessor tag-page-related-tags)
   (feed :accessor content-feed))
  (:documentation "Page for content matching a tag."))

(defmethod shared-initialize :after
    ((tag-page tag-page) slot-names &key &allow-other-keys)
  (with-slots (feed tag filename) tag-page
    (setf filename (%sanitise-title tag))
    (unless (and (slot-boundp tag-page 'feed) feed)
      (setf feed
            (make-instance 'atom-feed
                           :title (format nil "~A : ~A" *blog-title* tag)
                           :uri (url-for tag-page)
                           :content-page tag-page))))
  (%blog-add :tag-pages tag-page))


(defmethod relative-path-for ((tag-page tag-page))
  "Relative path for a tag page"
  *tag-page-path*)

(defmethod path-for ((tag-page tag-page))
  (format nil "~A~A~A.xhtml" *blog-root-path*
          (relative-namestring-for tag-page)
          (content-filename tag-page)))

(defmethod site-file-path-for ((tag-page tag-page))
  "Find the site file path for the specified TAG-PAGE."
  (ensure-directories-exist
   (merge-pathnames
    (make-pathname :directory (relative-directory-for tag-page)
                   :name (content-filename tag-page)
                   :type "xhtml") *site-path*)))

(defmethod content-title ((tag-page tag-page))
  (tag-page-tag tag-page))


;;;### Templated content
;;; All content that uses a template to merge user written content.
(defclass templated-content (generated-content)
  ((filename :initarg :filename :reader content-filename)
   (title :initarg :title :accessor content-title)
   (tags :initarg :tags :initform nil :accessor content-tags)
   (when :initarg :when :initform nil :reader content-when
         :type unsigned-byte
         :documentation "When post was originally written.")
   (updated :initarg :updated :initform nil :accessor content-updated
            :type unsigned-byte
            :documentation "Last update time")
   (description :initarg :description :initform nil
                :accessor content-description)
   (synopsis :initarg :synopsis :initform nil :accessor content-synopsis))
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


(defmethod published-file-path-for (published-content)
  "Find the publish file path for the specified TEMPLATED-CONTENT."
  (let ((type (cdr (assoc (type-of published-content)
                          *templated-content-file-types*))))
    (assert type)
    (ensure-directories-exist
     (merge-pathnames
      (make-pathname :directory (relative-directory-for published-content)
                     :name (content-filename published-content)
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
  (destructuring-bind (day month year)
      (decode-date (content-when templated-content))
    (declare (ignore day month))
    (values year)))




;;;### Page
;;; A user written page that is meant to be updated over time, and is not a blog post
(defclass page (templated-content)
  ((template :initarg :template :initform nil :reader content-template))
  (:documentation "Metadata for pages"))

(defmethod shared-initialize :after
    ((page page) slot-names &key &allow-other-keys)
  (%blog-add :pages page))

(defmethod relative-path-for ((page page))
  "Relative path for PAGE."
  *page-path*)


(defmethod link-for ((page page) &key url)
  (cxml:with-element "span"
    (cxml:attribute "class" "page-link")
    (cxml:with-element "a"
      (cxml:attribute *href* (or url (path-for page)))
      (cxml:text (content-title page)))))

;;;### Comment for a blog post
(defclass comment ()
  ((content-page :initarg :content-page :reader content-page)
   (filename :initarg :filename :reader content-filename)
   (ip :initarg :ip :reader comment-ip)
   (name :initarg :name :reader comment-name)
   (email :initarg :email :reader comment-email)
   (uri :initarg :uri :reader comment-uri)
   (when :initarg :when :reader comment-when))
  (:documentation "Metadata for comments"))

(defmethod shared-initialize :after
    ((comment comment) slot-names &key &allow-other-keys)
  (with-slots (filename name when) comment
    ;; default to current time
    (unless when
      (setf when (get-universal-time)))
    ;; create a sanitised filename
    (setf filename (format nil "~A_~A" when (%sanitise-title name))))
  (%blog-add :comments comment))

(defmethod relative-path-for ((comment comment))
  "Relative path for a comment."
  (append *comment-path*
          (list (content-filename (content-page comment)))))

(defmethod path-for ((comment comment))
  (concatenate 'simple-string (path-for (content-page comment))
               "#" (content-filename comment)))


;;;### Blog post
;;; A blog post is content with a template of "post" which is the default
;;; template.
(defclass blog-post (templated-content)
  ((template :initform *blog-post-template* :reader content-template
             :allocation :class)
   (comments :initform nil :accessor content-comments))
  (:documentation "Metadata for blog-posts"))

(defmethod shared-initialize :after
    ((blog-post blog-post) slot-names &key &allow-other-keys)
  (%blog-add :posts blog-post))

(defmethod print-object ((object blog-post) stream)
  "Print a blog post instance showing date and filename"
  (print-unreadable-object (object stream :type t)
    (with-slots (filename when) object
      (format stream "(~{~A~^-~}) ~A " (decode-date when) filename))))

(defmethod relative-path-for ((blog-post blog-post))
  "Relative path for a blog post"
  (append *blog-post-path*
          (list (format nil "~A" (content-year blog-post)))))

(defmethod link-for ((blog-post blog-post) &key url)
  (cxml:with-element "a"
    (cxml:attribute *href* (or url (path-for blog-post)))
    (cxml:text (content-title blog-post))))

(defmethod link-for ((tag-page tag-page) &key url)
  (cxml:with-element "a"
    (cxml:attribute *href* (or url (path-for tag-page)))
    (cxml:text (tag-page-tag tag-page))))

(defmethod content-feed ((blog-post blog-post))
  (content-feed (index-page)))

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
(defun scan-post (pathname)
  (format t "scan-post ~A~%" pathname)
  (multiple-value-bind
        (title post-when post-updated tags linkname description synopsis
               template)
      (%parse-post-info pathname)
    ;(format t "~A ~A ~A ~A ~A ~A~%" title post-when post-updated tags linkname template)
    (make-metadata title post-when post-updated tags linkname
                   description synopsis template)))

(defun scan-comment (pathname)
  (format t "scan-comment ~A~%" pathname)
  (multiple-value-bind (ip name email uri when)
      (%parse-comment pathname)
    (let* ((post-filename (car (last (pathname-directory pathname))))
           (filename (pathname-name pathname))
           (id (subseq filename 0 (position #\_ filename)))
           (post (%blog-find-by :posts 'content-filename post-filename))
           (comment (make-instance 'comment
                                   :content-page post
                                   :ip ip
                                   :id id
                                   :when when
                                   :name name
                                   :email email
                                   :uri uri)))
      (push comment (content-comments post)))))


(defun scan-directory (pathname)
  (format t "scan-directory ~A~%" pathname)
  (loop
     with post-type = (cdr (assoc 'blog-post *templated-content-file-types*))
     with page-type = (cdr (assoc 'page *templated-content-file-types*))
     with comment-type = (cdr (assoc 'comment *templated-content-file-types*))
     for file in (directory pathname)
     do
       (cond ((zerop (length (file-namestring file)))
              (scan-directory (make-pathname :name :wild :type :wild :defaults file)))
             ((string= post-type (pathname-type file))
              (scan-post file))
             ((string= page-type (pathname-type file))
              (scan-post file))
             ((string= comment-type (pathname-type file))
              (scan-comment file))
             (t
              (format t "unrecognised type ~A~%" (pathname-type file))))))

(defun scan-blog-files ()
  "Scan posts and build an in memory representation."
  (flet ((directory-for (path)
           (merge-pathnames
            (make-pathname :directory (apply 'list :relative path) :name :wild)
            *published-path*)))
    (setf *blog* (make-hash-table :test 'equalp))
    (setf (gethash :posts *blog*) (list))
    (setf (gethash :pages *blog*) (list))
    (setf (gethash :comments *blog*) (list))
    (setf (gethash :all *blog*) (list))

    (scan-directory (make-pathname :type :wild :defaults (directory-for *blog-post-path*)))
    (scan-directory (make-pathname :type :wild :defaults (directory-for *page-path*)))
    (scan-directory (make-pathname :type :wild :defaults (directory-for *comment-path*))))

  ;; *tag-page-path*
  ;; *comment-path*

  *blog*)

(defun %blog ()
  "Return the in memory representation of the blog"
  (or *blog* (scan-blog-files)))

(defun blog (key)
  (gethash key (%blog)))


(defun %recent-posts (&key (n 10))
  "Create a list of recent posts."
  (let ((posts (blog :posts)))
    (if (> (length posts) n)
        (subseq posts 0 n)
        posts)))

(defun %tag-posts (tag)
  "Create a list of recent posts for a given tag."
  (flet ((has-tag (post)
           (find tag (content-tags post) :test 'string=)))
    (remove-if-not #'has-tag (blog :posts))))

(defun %map-dirty-content (fn)
  "Map all dirty content for side effects only."
  (mapc fn (remove-if-not 'dirty (blog :all))))

(defun %adjacent-posts (blog-post)
  "Return post before and after the given post."
  (assert blog-post)
  (if (= (length (blog :posts)) 1)
      (values)
      (loop
         for m2 = nil then m1
         for m1 = nil then post
         for post in (blog :posts)
         until (eq blog-post m1)
         finally (return (if (eq post blog-post)
                             (values m2)
                             (values m2 post))))))

(defun %blog-find-by (kind key value)
  "find an item in the kind collection, with key = value"
  (find value (blog kind) :key key :test #'equal))

(defun %blog-remove (item)
  "Remove item"
  (remove item (blog :all)))

;;;# Publishing

;;; Generate a blog post from an input post file.  The post is read and a
;;; published post file is created.  Metadata for the post is read and stored in
;;; the database.  Optionally, the site is regenerated.  The output page for
;;; the post is always generated, so it may be proofed.
(defun publish-draft (path &key (generate-site nil))
  "Publish the draft post at the specified filesystem PATH. Returns a list with
the path to the published file and the site path."
  (multiple-value-bind (output-path templated-content)
      (%publish-draft (pathname path))
    (if generate-site
        (generate-site)
        (generate templated-content))
    (list (namestring output-path)
          (namestring (site-file-path-for templated-content))
          (url-for templated-content)
          (path-for templated-content))))

;;; Republishing uses the "updated" element in the "head" to set the updated time
;;; on the post.  If no "updated" is present, then one is added eith the current
;;; date.
(defun publish-updated-post (path &key (generate-site nil))
  "Publish an updated post at the specified filesystem PATH. Returns a list with
the path to the published file and the site path."
  (multiple-value-bind (output-path templated-content)
      (%publish-updated-post (pathname path))
    (if generate-site
        (generate-site)
        (generate templated-content))
    (list (namestring output-path)
          (namestring (site-file-path-for templated-content))
          (url-for templated-content)
          (path-for templated-content))))

;;;## Add a comment
;;; Adds comment metadata to the database and writes a published comment file
(defun add-comment (post-slug ip name email uri when text)
  "Create a comment TEXT on the post identified by POST-SLUG, originating from
  NAME with EMAIL and URI.  TEXT is plain text."
  (macrolet ((with-each-paragraph ((var text) &body body)
               (let ((paras (gensym)))
                 `(let ((,paras
                         (cl-ppcre:split "(?:\\r\\n|\\n|\\r){2,}" ,text)))
                    (loop for ,var in ,paras
                       do ,@body)))))
    (labels ((htmlify (para)
               (let ((i 0))
                 (cl-ppcre:do-scans
                     (ms me rs re "(https?|mailto)://[\\S-/]+/\\S*" para)
                   (when (> ms i)
                     (cxml:text (subseq para i ms)))
                   (let ((link (subseq para ms me)))
                     (cxml:with-element "a"
                       (cxml:attribute "href" link)
                       (cxml:attribute "rel" "nofollow")
                       (cxml:text link)))
                   (setf i me))
                 (when (< i (length para))
                   (cxml:text (subseq para i)))))
             (output-text-as-html (text)
               "Output text converted to simple HTML"
               (with-each-paragraph (paragraph text)
                 (cxml:with-element "p"
                   (htmlify paragraph)))))
      (let ((blog-post (%blog-find-by :posts #'content-filename post-slug)))
        (unless blog-post
          (error "Unknown post ~A" post-slug))
        (let ((comment
               (make-instance 'comment :content-page blog-post :ip ip
                              :name name :email email :uri uri :when when)))
          (push comment (content-comments blog-post))
          (setf (dirty blog-post) t)
          (with-open-file (stream (published-file-path-for comment)
                                  :element-type '(unsigned-byte 8)
                                  :direction :output
                                  :if-does-not-exist :create)
            (let ((output
                   (cxml:make-octet-stream-sink
                    stream :canonical nil :indentation nil
                    :omit-xml-declaration-p t)))
              (cxml:with-xml-output output
                (cxml:with-element (cdr (assoc 'comment *templated-content-file-types*))
                  (cxml:with-element *comment-ip*
                    (cxml:text ip))
                  (cxml:with-element *comment-name*
                    (cxml:text name))
                  (cxml:with-element *comment-email*
                    (cxml:text email))
                  (cxml:with-element *comment-uri*
                    (cxml:text uri))
                  (cxml:with-element *comment-when*
                    (cxml:text (local-time:format-rfc3339-timestring
                                nil (local-time:universal-to-timestamp when))))
                  (cxml:with-element *comment-text*
                    (output-text-as-html text))))))
          (values comment))))))

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
;;; info for it. Returns the published file path and the blog-post metadata.  If
;;; a post with the same title already exists, then we assume this is an error
;;; and raise a condition.  A restart is provided to delete the existing post
;;; and proceed.
(defun %publish-draft (path)
  "Publish the draft at the filesystem PATH."
  (multiple-value-bind
        (title post-when post-updated tags linkname description synopsis
               template)
      (%parse-post-info path)
    (let ((existing-post
           (or (%blog-find-by :posts #'content-title title)
               (%blog-find-by :pages #'content-title title))))
      (if existing-post
          (restart-case
              (error "This content already exists.")
            (delete-existing-entry ()
              (%blog-remove existing-post)))))


    (let ((content
           (make-metadata title post-when post-updated tags linkname
                          description synopsis template)))
      (let* ((output-path (published-file-path-for content)))
        (%publish-draft-updating-post-metadata path output-path content)
        (%ensure-tag-pages-for content)
        (%mark-connected-posts-dirty content)
        (values output-path content)))))



;;; Publish an updated post, adding an updated date. Returns the published file
;;; path and the blog-post metadata.
(defun %publish-updated-post (path)
  "Publish the updated post at the filesystem PATH."
  (multiple-value-bind
        (title post-when post-updated tags linkname description synopsis
               template)
      (%parse-post-info path)
    (declare (ignore post-when template))
    (let ((existing-post
           (or (%blog-find-by :posts #'content-filename linkname)
               (%blog-find-by :pages #'content-filename linkname))))
      (unless existing-post
        (error "This blog post (~A) can not be found with slug ~A.
Ensure that the linkname has not been changed." path linkname))
      (unless (equal (published-file-path-for existing-post) path)
        (error "This blog post is not in the expected location.
Ensure that the file has not been moved."))

      (unless post-updated
        (setf post-updated (decode-local-date (get-universal-time))))

      ;; update metadata
      (setf (content-updated existing-post) (encode-date post-updated))
      (setf (content-synopsis existing-post) synopsis)
      (setf (content-title existing-post) title)
      (setf (content-description existing-post) description)
      (setf (content-tags existing-post) tags)

      (let* ((output-path (make-pathname :type "tmp" :defaults path)))
        (%publish-draft-updating-post-metadata path output-path existing-post)
        (cl-fad:copy-file output-path path :overwrite t)
        (%ensure-tag-pages-for existing-post)
        (%mark-connected-posts-dirty existing-post)
        (values output-path existing-post)))))


;;; Parse the "title", "when", "updated", "linkname" and "tag" elements.  Also
;;; store the first paragraph of the post to act as a synopsis.
(defun %parse-post-info (path)
  "Parse the input file at PATH, extracting the metadata.  Returns title
when (day month year), updated (day month year), tags, linkname, description
and synopsis."
  (let (title tags post-when post-updated linkname description
              synopsis template)
    (labels ((decode-date (date-data)
               (when date-data
                 (loop for key in '("day" "month" "year")
                    collect (parse-integer
                             (cdr (assoc key date-data :test #'string=)))))))
      (klacks:with-open-source
          (post (cxml:make-source path :entity-resolver #'null-resolver))
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
               (when (string= *post-description*
                              (cdr (assoc "name" attribs :test #'string=)))
                 (setf description
                       (cdr (assoc "content" attribs :test #'string=))))))
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

(defun %parse-comment (path)
  "Parse the input file at PATH, extracting the metadata.  Returns ip, name, email, uri, when."
  (let (ip name email uri when
           (top-element (cdr (assoc 'comment *templated-content-file-types*))))
    (klacks:with-open-source
	(comment (cxml:make-source path :entity-resolver #'null-resolver))
      (loop
	 do
	 (multiple-value-bind (key ns element) (klacks:consume comment)
	   (cond
	     ((start-comment-element-p key ns element *comment-when*)
	      (setf when
		    (local-time:timestamp-to-universal
		     (local-time:parse-rfc3339-timestring
		      (klacks:consume-characters comment)))))
	     ((start-comment-element-p key ns element *comment-ip*)
	      (setf ip (klacks:consume-characters comment)))
	     ((start-comment-element-p key ns element *comment-name*)
	      (setf name (klacks:consume-characters comment)))
	     ((start-comment-element-p key ns element *comment-email*)
	      (setf email (klacks:consume-characters comment)))
	     ((start-comment-element-p key ns element *comment-uri*)
	      (setf uri (klacks:consume-characters comment)))
	     ((start-comment-element-p key ns element *comment-text*)
	      (klacks:consume-characters comment))
	     ((end-comment-element-p key ns element top-element)
	      (return nil)))))
      (values ip name email uri when))))

;;; When a blog post is published or changes, then some of the pages that link
;;; to the post will need to be updated.  This function finds all such pages and
;;; marks them dirty.  The index and atom-feed will need updating if the post is
;;; in the recent-posts list.
(defgeneric %mark-connected-posts-dirty (content)
  (:documentation "Mark connected objects as needing update"))

(defmethod %mark-connected-posts-dirty ((page page)))

(defmethod %mark-connected-posts-dirty ((blog-post blog-post))
  "Mark as dirty anything that the post should cause to be regenerated"
  (let ((recent-posts (%recent-posts)))
    (setf *recent-posts* recent-posts)
    (when (find blog-post recent-posts)
      (setf (dirty (index-page)) t))
    (multiple-value-bind (prior next) (%adjacent-posts blog-post)
      (when prior
        (setf (dirty prior) t))
      (when next
        (setf (dirty next) t)))))


(defun %mark-all-dirty ()
  "Mark everything as dirty."
  (flet ((make-dirty (x)
           (setf (dirty x) t)))
    (make-dirty (index-page)) t
    (mapc #'make-dirty (blog :all))
    (setf *recent-posts* nil)))


(defun %ensure-tag-page (tag &optional tags)
  (let ((tag-page (or (%blog-find-by :tag-pages #'tag-page-tag tag)
                      (make-instance 'tag-page :tag tag))))
    (setf (dirty tag-page) t)
    (when tags
      (setf (tag-page-related-tags tag-page)
            (delete tag
                    (remove-duplicates
                     (merge 'list (tag-page-related-tags tag-page)
                            (copy-seq tags) #'string<)
                     :test #'string=)
                    :test #'string=)))
    (values tag-page)))

(defun %ensure-tag-pages-for (content)
  "Ensure tag pages exist for the tags in content."
  (loop
     with tags = (content-tags content)
     for tag in tags
     do (%ensure-tag-page tag tags)))

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
  "Copy the source inserting the post-when info.  If the output file exists
then it is overwritten (if the user has not chosen to delete an existing post
then this code will not be executed)."
  (klacks:with-open-source
      (draft (cxml:make-source path :entity-resolver #'null-resolver))
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
                   (funcall
                    (cdr (assoc name *metadata-dispatch-table*
                                :test #'string=))
                    blog-post)))
          (with-xml-fragment-output output
            (loop do
                 (multiple-value-bind (key ns lname) (klacks:peek draft)
                   (cond
                     ((null key)
                      (restart-case
                          (error "Error outputing updated post.
Please validate your editted post.")
                        (return-from-output () (return nil))))
                     ((and (eql key :start-element)
                           (find lname elements-to-process :test #'string=))
                      (setf elements-to-process
                            (delete lname elements-to-process :test #'string=))
                      (write-element lname)
                      (%suppress-current-element draft))
                     ((start-post-element-p key ns lname *post-meta*)
                      (let ((attribs (%capture-attributes draft)))
                        (if (string=
                             *post-description*
                             (cdr (assoc "name" attribs :test #'string=)))
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
  "Generate all dirty content for the site.  Assumes an existing database
connection."
  (index-page) ; ensure we have one
  (let (*recent-posts*)
    (%map-dirty-content #'generate)))

(defun generate-site (&key all)
  "Generate all dirty content for the site. Creates a database connection.  When
passed :ALL, will mark everything as dirty and regenerate (useful if you change
the templates)."
  (when all
    (%mark-all-dirty))
  (%generate-site)
  (list (url-for (index-page))
        (path-for (index-page))))

;;;## Output Macros

;;; Used to output the current element from the template, executing the macro's
;;; body before writing the end element tag
(defmacro with-existing-element ((template output) &body body)
  "Ouput the template, executing body before closing the current element."
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

;;; Ouput the template, executing body before closing the current element.  The
;;; content of the template element is expected to be a format template and is
;;; not output verbatim.
(defmacro with-existing-element-as-format-template
    ((format-var template output) &body body)
  "Execute BODY with the current element, assigning the inner content of the
element to FORMAT-VAR."
  (let ((tp (gensym))
        (o (gensym))
        (k (gensym))
        (d (gensym)))
    `(let ((,tp ,template)
           (,o ,output)
           (,format-var ""))
       (klacks:serialize-event ,tp ,o)
       (loop for (,k ,d) = (multiple-value-list (klacks:peek ,tp))
          while (not (eql ,k :end-element))
          do (when (eql ,k :characters)
               (setf ,format-var (concatenate 'simple-string ,format-var ,d)))
            (klacks:consume-characters ,tp))
       ,@body
       (klacks:serialize-event ,tp ,o))))



;;; Ouput the template, executing body before closing the current element.  The
;;; content of the template element is expected to be a format template and is
;;; not output verbatim.
(defmacro with-existing-element-setting-attributes
    ((content template output old-attributes new-attributes) &body body)
  "Execute BODY with the current element, ensuring that the specified
NEW-ATTRIBUTES are merged with OLD-ATTRIBUTES and appear on the element."
  (let ((co (gensym))
        (tp (gensym))
        (o (gensym))
        (oa (gensym))
        (na (gensym))
        (k (gensym))
        (ns (gensym))
        (d (gensym)))
    `(let ((,co ,content)
           (,tp ,template)
           (,o ,output)
           (,oa ,old-attributes)
           (,na ,new-attributes))
       (multiple-value-bind (,k ,ns ,d) (klacks:consume ,tp)
         (declare (ignore ,ns))
         (assert (eql ,k :start-element))
         (cxml:with-element ,d
           (mapc #'(lambda (x) (cxml:attribute (car x) (cdr x)))
                 (merge-assoc ,oa ,na :test #'string=))
           (cxml:text "")
           (output-with-rewrite ,co ,tp ,o)
           ,@body))
       (klacks:consume ,tp))))


;;;## Output Functions


;;;### Template Helper Functions

;;; Create a template from the current element.  To be valid XML the template
;;; needs to include a wrapping element, but this will not be output.
(defun current-element-as-template (source)
  "Create an octect vector from the content of the current element"
  (let ((template-sink
         (cxml:make-octet-vector-sink :canonical nil :indentation
                                      nil :omit-xml-declaration-p t)))
    (cxml:with-xml-output template-sink
      (cxml:with-element "div"
        (cxml:text "") ; force output of div
        (%copy-current-element-content source template-sink)
        (sax:end-document template-sink)))))

(defun output-content-using-template (content template output)
  (klacks:with-open-source
      (source (cxml:make-source template :entity-resolver #'null-resolver))
    (loop for key =  (klacks:consume source)
         until (eql key :start-element)) ; consume the start element
    (output-with-rewrite content source output)
    (klacks:consume source))) ; consume the end element

;;;### Post and Page Output Functions
(defun output-post-content-no-template (blog-post output)
  (klacks:with-open-source
      (source (cxml:make-source (published-file-path-for blog-post)
                                :entity-resolver #'null-resolver))
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

(defun output-post-meta-link (content template output attributes)
  "Output link element for feed"
  (let ((rel (assoc "rel" attributes :test #'string=))
        (type (assoc "type" attributes :test #'string=)))
    (if (and (string= *rel-alternate* (cdr rel))
             (string= *atom-mime* (cdr type)))
        (progn
          (%suppress-current-element template)
          (cxml:with-element *post-link*
            (setf (cdr (assoc "href" attributes :test #'string=))
                  (namestring (path-for (content-feed content))))
            (mapc #'(lambda (x)
                      (cxml:attribute (car x) (cdr x)))
                  attributes)))
        (with-existing-element (template output)))))

(defun output-post-when (blog-post template output attributes)
  (declare (ignore attributes))
  (with-existing-element (template output)
    (cxml:text (format nil "~{~A~^-~}"
                       (decode-date (content-when blog-post))))))

(defun output-post-updated (blog-post template output attributes)
  (declare (ignore attributes))
  (with-existing-element (template output)
    (let ((updated (content-updated blog-post)))
      (when updated
        (cxml:with-element "span"
          (cxml:attribute "id" "post-updated-date")
          (cxml:text (format nil "~{~A~^-~}" (decode-date updated))))))))

(defun write-post-synopsis (blog-post output)
  "Output the synopsis"
  (klacks:with-open-source
      (source (cxml:make-source (content-synopsis blog-post)
                                :entity-resolver #'null-resolver))
    (%skip-preamble source)
    (output-with-rewrite blog-post source output)))



(defparameter *index-collection* nil)



(defun output-link-for (blog-post template output attributes)
  "Output a link for the specified post or page specified in the href
attribute."
  (declare (ignore blog-post))
  (let* ((href-entry (assoc *href* attributes :test #'string=))
         (linkname (%sanitise-title (cdr href-entry)))
         (linked-content
          (some #'(lambda (class)
                    (%blog-find-by class #'content-filename linkname))
                '(:pages :posts))))
    (when linked-content
      (setf (cdr href-entry) (url-for linked-content))))
  (klacks:serialize-event template output)
  (cxml:with-element "a"
    (mapc #'(lambda (x)
              (cxml:attribute (car x) (cdr x)))
          attributes)
    (cxml:text "")
    (%copy-current-element-content template output)
    (klacks:serialize-event template output)))

(defun output-post-tag (tag template output attributes)
  (declare (ignore attributes))
  (with-existing-element (template output)
    (let ((tag-page (%ensure-tag-page tag)))
      (link-for tag-page))))

(defun output-tags-for (content template output attributes)
  (declare (ignore attributes))
  (klacks:serialize-event template output) ; start element
  (let ((tag-template (current-element-as-template template)))
    (flet ((output-tag (tag)
             (output-content-using-template tag tag-template output)))
      (mapc #'output-tag (content-tags content))))
  (klacks:serialize-event template output))

(defun output-tag-name (content template output attributes)
  (declare (ignore attributes))
  (with-existing-element (template output)
    (cxml:text (tag-page-tag content))))


(defun output-tag-related (content template output attributes)
  (declare (ignore attributes))
  (let ((tags (tag-page-related-tags content)))
    (flet ((output-post-tag (tag)
             (cxml:with-element "li"
               (let ((tag-page (%ensure-tag-page tag tags)))
                 (link-for tag-page)))))
      (with-existing-element (template output)
        (cxml:with-element "ul"
          (mapc #'output-post-tag tags))))))

(defvar *collection-route-path* nil)

(defun output-post-synopsis (blog-post template output attributes)
  "Output a link to the specified BLOG-POST, using the post's title as link
  text."
  (declare (ignore attributes))
  (with-existing-element (template output)
    (write-post-synopsis blog-post output)))

;;; If the current element is not a link, insert a link as content, else
;;; set the href of the link.
(defun output-post-link (blog-post template output attributes)
  "Output a link to the specified BLOG-POST, using the post's title as link
  text."
  (multiple-value-bind (key ns lname) (klacks:peek template)
    (declare (ignore ns))
    (assert (eql key :start-element))
    (if (string= "a" lname)
        (with-existing-element-setting-attributes
            (blog-post template output attributes
                       (list (cons *href* (path-for blog-post)))))
        (with-existing-element (template output)
          (if nil                        ;        *collection-route-path*
              (link-for blog-post
                        :url (enough-namestring
                              (path-for blog-post)
                              *collection-route-path*))
              (link-for blog-post))))))

(defun output-delicious-link (blog-post template output attributes)
  "Modify an anchor element to point to delicious post."
  (with-existing-element-setting-attributes
      (blog-post
       template output attributes
       (list (cons *href*
                   (format nil "http://del.icio.us/post?url=~A&title=~A"
                           (url-for blog-post)
                           (content-title blog-post)))))))

(defun output-digg-link (blog-post template output attributes)
  "Modify an anchor element to point to digg post."
  (with-existing-element-setting-attributes
      (blog-post
       template output attributes
       (list (cons *href*
                   (format nil "http://digg.com/submit?phase=2&url=~A&title=~A"
                           (url-for blog-post)
                           (content-title blog-post)))))))

(defun output-posts (content template output attributes)
  "Output posts"
  (declare (ignore attributes))
  (klacks:serialize-event template output) ; start element
  (let ((post-template (current-element-as-template template)))
    (flet ((output-post (post)
             (output-content-using-template post post-template output)))
      (let ((*collection-route-path* (path-for content)))
        (mapc #'output-post *index-collection*))))
  (klacks:serialize-event template output)) ; end element


(defun output-post-slug-value-attr (content template output attributes)
  "Output a value attribute containing the post slug"
  (if (assoc "value" attributes :test #'string=)
      (setf (cdr (assoc "value" attributes :test #'string=))
            (content-filename content))
      (setf attributes (acons "value" (content-filename content) attributes)))
  (multiple-value-bind (key ns lname) (klacks:consume template)
    (declare (ignore key ns))
    (cxml:with-element lname
      (mapc #'(lambda (x)
                (cxml:attribute (car x) (cdr x)))
            attributes)
      (%copy-current-element-content template output))
    (klacks:consume template)))

;;;### Comment Output Functions

(defun split-fmt (fmt)
  (when fmt
    (setf fmt (string-trim '(#\Space #\Tab #\Newline #\Return)  fmt))
    (when (plusp (length fmt))
      (let ((i 0) (result) (n (length fmt)))
        (cl-ppcre:do-scans (ms me rs re "\\|" fmt)
          (push (subseq fmt i ms) result)
          (setf i me)
          (when (= n me)
            (push "" result)))
        (when (< i n)
          (push (subseq fmt i) result))
        (values (nreverse result))))))

(defun output-comment-count (content template output attributes)
  "Output a count of the comments for content"
  (declare (ignore attributes))
  (flet ((write-fmt (template count)
            (cl-ppcre:regex-replace "#" template (format nil "~D" count))))
    (with-existing-element-as-format-template (fmt template output)
      (setf fmt (split-fmt fmt))
      (cxml:text
       (if fmt
           (let ((components (length fmt))
                 (count (length (content-comments content))))
             (cond
               ((= 1 components)
                (write-fmt (first fmt) count))
               ((= 2 components)
                (if (= 1 count)
                    (write-fmt (second fmt) count)
                    (write-fmt (first fmt) count)))
               (t
                (if (= 1 count)
                    (write-fmt (second fmt) count)
                    (if (zerop count)
                        (write-fmt (third fmt) count)
                        (write-fmt (first fmt) count))))))
           (format nil "~D" (length (content-comments content))))))))


(defun output-comments (content template output attributes)
  (declare (ignore attributes))
  (klacks:serialize-event template output) ; start element
  (let ((comment-template (current-element-as-template template)))
    (flet ((output-comment (comment)
             (output-content-using-template comment comment-template output)))
      (mapc #'output-comment (content-comments content))))
  (klacks:serialize-event template output))  ; end element

(defun output-comment (content template output attributes)
  (declare (ignore attributes))
  (with-open-file (stream (published-file-path-for content)
                          :element-type '(unsigned-byte 8))
    (klacks:with-open-source (source (cxml:make-source stream))
      (%skip-to-next-start-element source "text")
      (with-existing-element (template output)
        (%copy-current-element-content source output)))))

(defun output-comment-from (content template output attributes)
  (declare (ignore attributes))
  (with-existing-element (template output)
    (cxml:with-element "a"
      (cxml:attribute "href" (comment-uri content))
      (cxml:text (comment-name content)))))

(defun output-comment-when (content template output attributes)
  (declare (ignore attributes))
  (with-existing-element (template output)
    (cxml:text
     (local-time:format-timestring
      nil
      (local-time:universal-to-timestamp (comment-when content))
      :format
      '(:day #\Space :long-month #\Space :year #\, #\Space :hour #\: :min)))))

(defun output-comment-link (content template output attributes)
  (declare (ignore attributes))
  (with-existing-element-as-format-template (fmt template output)
    (cxml:with-element "a"
      (cxml:attribute "href" (url-for content))
      (cxml:text fmt))))

(defun output-comment-id (content template output attributes)
  (with-existing-element-setting-attributes
      (content template output attributes
                (list (cons "id" (content-filename content))))))


;;;### Feed Output functions
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

;;;### Template dispatch tables
;;; Table mapping element id's to the corresponding output function
(defparameter *id-dispatch-table*
  (list
    (cons *post-content-id*  #'output-post-content)
    (cons *post-title-id*  #'output-post-title)
    (cons *post-when-id* #'output-post-when)
    (cons *post-updated-id*  #'output-post-updated))
  "Map elements with specific id atributes to content.")

;;; This maps the template class-id's to corresponding output functions
(defparameter *class-dispatch-table*
  (list
   (cons *post-posts-class*  #'output-posts)
   (cons *post-synopsis-class*  #'output-post-synopsis)
   (cons *post-link-class*  #'output-post-link)
   (cons *page-link-for-class* #'output-link-for)
   (cons *post-tags-class* #'output-tags-for)
   (cons *post-tag-class* #'output-post-tag)
   (cons *tag-name-class* #'output-tag-name)
   (cons *tag-related-class* #'output-tag-related)
   (cons *comment-count-class* #'output-comment-count)
   (cons *comments-class* #'output-comments)
   (cons *comment-class* #'output-comment)
   (cons *comment-from-class* #'output-comment-from)
   (cons *comment-when-class* #'output-comment-when)
   (cons *comment-id-class* #'output-comment-id)
   (cons *comment-link-class* #'output-comment-link)
   (cons *delicious-class* #'output-delicious-link)
   (cons *digg-class* #'output-digg-link)
   (cons *post-slug-hidden-input-class* #'output-post-slug-value-attr))
  "Map elements with specific class atributes to content.")

(defparameter *element-dispatch-table*
  (list
   (cons *post-title* #'output-post-title)
   (cons *post-link* #'output-post-meta-link))
  "Map specific elements to content.")

(defparameter *feed-element-dispatch-table*
  (list
   (cons *feed-title* #'output-feed-title)
   (cons *feed-uri* #'output-feed-uri)
   (cons *feed-updated* #'output-feed-updated))
  "Map specific elements to content.")


;;;### Template Engine
;;; A helper macro.
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

;;; The main template rewriting function.
(defun output-with-rewrite (templated-content source output)
  "Outputs a SOURCE, rewriting content as required to the OUTPUT sink.  Writes
only the current element."
  (labels ((dispatch (cmd attributes)
             (when cmd
               (funcall (cdr cmd)
                        templated-content source output attributes)))
           (element-cmd (lname)
             (assoc lname *element-dispatch-table* :test #'string=))
           (id-cmd (attributes)
             (assoc (cdr (assoc "id" attributes :test #'string=))
                    *id-dispatch-table* :test #'string=))
           (class-cmd (attributes)
             (with-each-word
                 (c (cdr (assoc "class" attributes :test #'string=)))
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

(defmethod generate ((tag-page tag-page))
  (let ((tag-posts (%tag-posts (tag-page-tag tag-page))))
    (generate-page tag-page :collection tag-posts)
    (generate-page (content-feed tag-page) :collection tag-posts)))


;;;### Templated Pages
;;; Generates a BLOG-POST or a PAGE.
(defmethod generate-page ((templated-content templated-content)
                          &key next prior &allow-other-keys)
  "Generate the page for a templated-content.  Takes the template and merges the
post into it."
  (format t "Generating post '~A'~%" (content-title templated-content))
  (flet ((output-post-head-title (template output)
           "Assusmes current element is the template title"
           ;; consume title tag and title text
           (loop for key = (klacks:peek template)
              until (eql key :end-element)
              do (klacks:serialize-event template output))
           (cxml:text (content-title templated-content))
           ;; close the title tag
           (klacks:serialize-event template output))
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
               (cxml:attribute
                "content"
                (format nil "~A" (content-description templated-content))))))
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
          (template
           (cxml:make-source
            (%template-path (content-template templated-content))
            :entity-resolver #'null-resolver))
        (with-open-file (stream output-path :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (let ((output
                 (cxml:make-octet-stream-sink
                  stream :canonical nil :omit-xml-declaration-p nil)))
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



;;;### Index Page
;;; Generates the index page using a list of recent posts, and listing
;;; links to them with a synopsis of each (the first paragraph, anyway).
(defmethod generate-page ((index-page index-page) &key collection)
  "Generate the index page for the blog."
  (format t "Generating index page~%")
  (klacks:with-open-source
      (template (cxml:make-source (%template-path "index")
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

;;;### Tag Pages
;;; Generates a tag page using a list of posts, and listing
;;; links to them with a synopsis of each (the first paragraph, anyway).
(defmethod generate-page ((tag-page tag-page) &key collection)
  "Generate the index page for the blog."
  (format t "Generating tag page ~A~%" (tag-page-tag tag-page))
  (klacks:with-open-source
      (template (cxml:make-source (%template-path "tag")
                                  :entity-resolver #'null-resolver))
    (with-open-file
        (stream (site-file-path-for tag-page) :direction :output
                :element-type '(unsigned-byte 8) :if-exists :supersede)
      (let* ((output (cxml:make-octet-stream-sink
                      stream :canonical nil :omit-xml-declaration-p nil)))
        (with-xml-fragment-output output
          (let ((*index-collection* collection))
            (loop for key = (klacks:peek template)
               while key
               until (eql key :start-element)
               do (klacks:serialize-event template output))
            (output-with-rewrite tag-page template output))))))
  (setf (dirty tag-page) nil))


;;;### Atom Feeds
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
        (write-post-synopsis blog-post output)))
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
  (klacks:with-open-source
      (template (cxml:make-source (%template-path "atom")
                                  :entity-resolver #'null-resolver))
    (with-open-file
        (stream (site-file-path-for atom-feed) :direction :output
                :element-type '(unsigned-byte 8) :if-exists :supersede)
      (let* ((output (cxml:make-octet-stream-sink
                      stream :canonical nil :indentation nil
                      :omit-xml-declaration-p nil)))
        (with-xml-fragment-output output
          (%copy-to-next-start-element template output "feed")
          (klacks:serialize-event template output)
          (let ((*element-dispatch-table* *feed-element-dispatch-table*))
            (output-with-rewrite atom-feed template output))
          (loop for blog-post in collection
             do (output-post-atom-entry blog-post output))
          (klacks:serialize-source template output))))))

;;;# Default configuration
;;; Defaults for configurable paths and id's
(eval-when (:load-toplevel :execute)
  (setf *blog-post-relative-path-fn* #'relative-path-for)
  (setf *page-relative-path-fn* #'relative-path-for)
  (setf *id-generator-fn* #'url-for))

;;; Hacks

(defun blog-index-page ()
  (site-file-path-for (index-page)))

;; (defun ensure-tag-pages-for-existing-blog-posts ()
;;   "Ensure tag pages exist for existing posts"
;;   (with-open-store ()
;;     (elephant:map-class #'%ensure-tag-pages-for 'blog-post)))

;; (defun ensure-comments-for-existing-blog-posts ()
;;   "Ensure tag pages exist for existing posts"
;;   (with-open-store ()
;;     (flet ((ensure-comments-for-post (post)
;;              (when (not (slot-boundp post 'comments))
;;                (setf (slot-value post 'comments) (elephant:make-btree)))))
;;       (elephant:map-class #'ensure-comments-for-post 'blog-post))))
;;; ensure pbook output is as intended:

;; Local Variables:
;; pbook-author:  "Hugo Duncan"
;; pbook-use-toc: t
;; pbook-style:   article
;; End:
