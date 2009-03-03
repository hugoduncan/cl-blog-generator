(in-package #:cl-blog-generator-test)

(declaim (optimize (debug 3) (safety 2) (speed 1)))
(in-suite test)


;;;# Basic Helpers
(defparameter *sane-tests*
  '(("this is a simple title" "this_is_a_simple_title")
    ("This is a Capitalised title" "this_is_a_capitalised_title")
    ("This case's title has, (in-correct), punctuation!." "this_cases_title_has_incorrect_punctuation")))

(deftest test-%sanitise-title ()
  (loop
     for case in *sane-tests*
     for (input expected) = case
     do
       (is (string= expected (cl-blog-generator::%sanitise-title input)))))


(defmacro with-test-db (&body body)
`(progn
   (configure :test)
   (elephant:with-open-store (*blog-db-spec*)
     (elephant:with-transaction ()
       ,@body))))

;;;# Fixtures to revert output state
(defun drop-all ()
  (with-test-db
    (elephant:drop-instances
     (elephant:get-instances-by-class (find-class 'cl-blog-generator::blog-post)))
    (elephant:drop-instances
     (elephant:get-instances-by-class 'cl-blog-generator::index-page))
    (elephant:drop-instances
     (elephant:get-instances-by-class 'cl-blog-generator::atom-feed))
    (elephant:drop-instances
     (elephant:get-instances-by-class 'cl-blog-generator::tag-page))
    (elephant:drop-instances
     (elephant:get-instances-by-class 'cl-blog-generator::generated-content)
     )))

(defun draft-path (filename)
  (merge-pathnames
   (make-pathname :directory '(:relative ".." "drafts") :name filename :type "post")
   *site-path*))

(deftest test-draft-path ()
  (let ((*site-path* #p"/a/site/"))
    (is (string= (namestring (draft-path "fred")) "/a/site/../drafts/fred.post"))))

(defixture delete-all-fixture
    (:setup
     (drop-all)
     (cl-fad:walk-directory *site-path* #'delete-file)
     (cl-fad:walk-directory *published-path* #'delete-file)))


(deftest test-%parse-post-info-first ()
  (with-fixture delete-all-fixture
    (with-test-db
      (multiple-value-bind (title when tags synopsis)
	  (cl-blog-generator::%parse-post-info (draft-path "first"))
	(declare (ignore synopsis))
	(is (string= title "My First Blog Post"))
	(is (equalp '(24 02 2009) when))
	(is (equalp '("lisp" "blog") tags))
	;; need to work out the type of the sysnopsis...
	;; (is (string= "<p>My first post.  Mainly to have something to use in developing the code.</p>" synopsis))
	))))


(deftest test-%publish-draft-first ()
  (with-fixture delete-all-fixture
    (with-test-db
      (multiple-value-bind (output-path blog-post)
	  (cl-blog-generator::%publish-draft (draft-path "first"))
	(is (cl-fad:file-exists-p output-path))
	(is (string= "My First Blog Post" (cl-blog-generator::blog-post-title blog-post)))
	(is (equalp '(24 02 2009)
		    (cl-blog-generator::decode-date
		     (cl-blog-generator::blog-post-when blog-post))))))))

(deftest test-publish-draft-first ()
  (with-fixture delete-all-fixture
    (destructuring-bind (pf sf)
	(publish-draft (draft-path "first"))
      (let ((sfe (format nil "~Apost/my_first_blog_post.xhtml" cl-blog-generator::*site-path*))
	    (pfe (format nil "~Amy_first_blog_post.post" cl-blog-generator::*published-path*)))
	(is (string= sfe (namestring sf)))
	(is (string= pfe (namestring pf)))
	(is (cl-fad:file-exists-p sfe))
	(is (cl-fad:file-exists-p pfe))))))