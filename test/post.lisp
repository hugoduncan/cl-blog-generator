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

(defixture test-environment-fixture
    (:setup
     (configure :test)))

(defun synopsis= (a b)
  (declare (ignore a b))
  ;; need to work out the type of the sysnopsis...
  ;; (is (string= "<p>My first post.  Mainly to have something to use in developing the code.</p>" synopsis))
  t)

(deftest test-%parse-post-info-first ()
  (with-fixture test-environment-fixture
    (multiple-value-bind (title when updated tags linkname synopsis)
	(cl-blog-generator::%parse-post-info (draft-path "first"))
      (is (string= title "My First Blog Post"))
      (is (equalp '(24 02 2009) when))
      (is (equalp '("lisp" "blog") tags))
      (is (not linkname))
      (is (not updated))
      (is (synopsis= "<p>My first post.  Mainly to have something to use in developing the code.</p>" synopsis))
      )))

(deftest test-%parse-post-info-second ()
  (with-fixture test-environment-fixture
    (multiple-value-bind (title when updated tags linkname synopsis)
	(cl-blog-generator::%parse-post-info (draft-path "second"))
      (is (string= title "My Second Blog Post"))
      (is (equalp '(26 02 2009) when))
      (is (equalp '("lisp" "blog") tags))
      (is (string= linkname "a_second_blog_post_with_an_explicit_linkname"))
      (is (equalp '(27 02 2009) updated))
      (is (synopsis= "<p>My second post.  With an explicit linkname, and an updated tag.</p>" synopsis))
      )))


(deftest test-%publish-draft-first ()
  (with-fixture delete-all-fixture
    (with-test-db
      (multiple-value-bind (output-path blog-post)
	  (cl-blog-generator::%publish-draft (draft-path "first"))
	(is (cl-fad:file-exists-p output-path))
	(is (string= "My First Blog Post"
		     (cl-blog-generator::blog-post-title blog-post)))
	(is (string= "my_first_blog_post"
		     (cl-blog-generator::blog-post-filename blog-post)))
	(is (equalp '(24 02 2009)
		    (cl-blog-generator::decode-date
		     (cl-blog-generator::blog-post-when blog-post))))
	(is (null (cl-blog-generator::blog-post-updated blog-post)))
	(is (string= "http://hugoduncan.org/blog/post/2009/my_first_blog_post.xhtml"
		     (funcall cl-blog-generator::*id-generator-fn* blog-post)))

	(multiple-value-bind (title when updated tags linkname synopsis)
	    (cl-blog-generator::%parse-post-info output-path)
	  (is (string= title "My First Blog Post"))
	  (is (equalp '(24 02 2009) when))
	  (is (equalp '("lisp" "blog") tags))
	  (is (string= linkname "my_first_blog_post"))
	  (is (null updated))
	  (is (synopsis= "<p>My first post.  Mainly to have something to use in developing the code.</p>" synopsis))
	  )))))




(deftest test-%publish-draft-second ()
  (with-fixture delete-all-fixture
    (with-test-db
      (multiple-value-bind (output-path blog-post)
	  (cl-blog-generator::%publish-draft (draft-path "second"))
	(is (cl-fad:file-exists-p output-path))
	(is (string= "My Second Blog Post"
		     (cl-blog-generator::blog-post-title blog-post)))
	(is (string= "a_second_blog_post_with_an_explicit_linkname"
		     (cl-blog-generator::blog-post-filename blog-post)))
	(is (equalp '(26 02 2009)
		    (cl-blog-generator::decode-date
		     (cl-blog-generator::blog-post-when blog-post))))
	(is (equalp '(27 02 2009)
		    (cl-blog-generator::decode-date
		     (cl-blog-generator::blog-post-updated blog-post))))
	(is (string= "http://hugoduncan.org/blog/post/2009/a_second_blog_post_with_an_explicit_linkname.xhtml"
		     (funcall cl-blog-generator::*id-generator-fn* blog-post)))))))



(deftest test-%publish-draft-and-%publish-updated-post-first ()
  (with-fixture delete-all-fixture
    (with-test-db
      (multiple-value-bind (output-path blog-post)
	  (cl-blog-generator::%publish-draft (draft-path "first"))
	(declare (ignore blog-post))
	(is (cl-fad:file-exists-p output-path))

	(multiple-value-bind (output-path blog-post)
	    (cl-blog-generator::%publish-updated-post output-path)
	  (let ((post-updated ;; this could cause problems testing at midnight
		 (cl-blog-generator::decode-local-date (get-universal-time))))
	    (is (cl-fad:file-exists-p output-path))
	    (is (string= "My First Blog Post"
			 (cl-blog-generator::blog-post-title blog-post)))
	    (is (string= "my_first_blog_post"
			 (cl-blog-generator::blog-post-filename blog-post)))
	    (is (equalp '(24 02 2009)
			(cl-blog-generator::decode-date
			 (cl-blog-generator::blog-post-when blog-post))))
	    (is (equalp post-updated
			(cl-blog-generator::decode-date
			 (cl-blog-generator::blog-post-updated blog-post))))
	    (is (string= "http://hugoduncan.org/blog/post/2009/my_first_blog_post.xhtml"
			 (funcall cl-blog-generator::*id-generator-fn* blog-post)))

	    (multiple-value-bind (title when updated tags linkname synopsis)
		(cl-blog-generator::%parse-post-info output-path)
	      (is (string= title "My First Blog Post"))
	      (is (equalp '(24 02 2009) when))
	      (is (equalp '("lisp" "blog") tags))
	      (is (string= linkname "my_first_blog_post"))
	      (is (equalp post-updated updated))
	      (is (synopsis=
		   "<p>My first post.  Mainly to have something to use in developing the code.</p>"
		   synopsis)))))))))


(deftest test-%publish-draft-and-%publish-updated-post-second ()
  (with-fixture delete-all-fixture
    (with-test-db
      (multiple-value-bind (output-path blog-post)
	  (cl-blog-generator::%publish-draft (draft-path "second"))
	(declare (ignore blog-post))
	(is (cl-fad:file-exists-p output-path))

	(multiple-value-bind (output-path blog-post)
	    (cl-blog-generator::%publish-updated-post output-path)
	  (is (cl-fad:file-exists-p output-path))
	  (is (string= "My Second Blog Post"
		       (cl-blog-generator::blog-post-title blog-post)))
	  (is (string= "a_second_blog_post_with_an_explicit_linkname"
		       (cl-blog-generator::blog-post-filename blog-post)))
	  (is (equalp '(26 02 2009)
		      (cl-blog-generator::decode-date
		       (cl-blog-generator::blog-post-when blog-post))))
	  (is (equalp '(27 02 2009)
		      (cl-blog-generator::decode-date
		       (cl-blog-generator::blog-post-updated blog-post))))
	  (is (string= "http://hugoduncan.org/blog/post/2009/a_second_blog_post_with_an_explicit_linkname.xhtml"
		       (funcall cl-blog-generator::*id-generator-fn* blog-post)))


	  (multiple-value-bind (title when updated tags linkname synopsis)
	      (cl-blog-generator::%parse-post-info output-path)
	    (is (string= title "My Second Blog Post"))
	    (is (equalp '(26 02 2009) when))
	    (is (equalp '("lisp" "blog") tags))
	    (is (string= linkname "a_second_blog_post_with_an_explicit_linkname"))
	    (is (equalp '(27 02 2009) updated))
	    (is (synopsis=
		 "<p>My second post.  With an explicit linkname, and an updated tag.</p>"
		 synopsis))))))))


(deftest test-%adjacent-posts ()
  (with-fixture delete-all-fixture
    (with-test-db
      (multiple-value-bind (output-path blog-post1)
	  (cl-blog-generator::%publish-draft (draft-path "first"))
	(declare (ignore output-path))
	(multiple-value-bind (output-path blog-post2)
	    (cl-blog-generator::%publish-draft (draft-path "second"))
	  (declare (ignore output-path))
	  (multiple-value-bind (output-path blog-post3)
	      (cl-blog-generator::%publish-draft (draft-path "third"))
	    (declare (ignore output-path))
	    (multiple-value-bind (prior next)
		(cl-blog-generator::%adjacent-posts blog-post1)
	      (is (null prior))
	      (is (eql next blog-post3)))
	    (multiple-value-bind (prior next)
		(cl-blog-generator::%adjacent-posts blog-post2)
	      (is (eql blog-post3 prior))
	      (is (null next)))
	    (multiple-value-bind (prior next)
		(cl-blog-generator::%adjacent-posts blog-post3)
	      (is (eql blog-post1 prior))
	      (is (eql blog-post2 next )))
	    ))))))

(deftest test-%generate-site ()
  (with-fixture delete-all-fixture
    (cl-blog-generator::publish-draft (draft-path "first"))
    (cl-blog-generator::publish-draft (draft-path "second"))
    (cl-blog-generator::publish-draft (draft-path "third"))
    (with-test-db
      (let ((index-page (cl-blog-generator::site-file-path-for
			 (cl-blog-generator::index-page))))
	(is (not (cl-fad:file-exists-p index-page)))
	(cl-blog-generator::%generate-site)
	(is (cl-fad:file-exists-p index-page))))))

(deftest test-generate-site ()
  (with-fixture delete-all-fixture
    (cl-blog-generator::publish-draft (draft-path "first"))
    (cl-blog-generator::publish-draft (draft-path "second"))
    (cl-blog-generator::publish-draft (draft-path "third"))
    (destructuring-bind (url path) (cl-blog-generator::generate-site)
      (is (stringp url))
      (is (stringp path))
      (with-test-db
	(let ((index-page
	       (cl-blog-generator::url-for
		(cl-blog-generator::index-page))))
	  (is (string= url index-page)))))))

(deftest test-publish-draft-first ()
  (with-fixture delete-all-fixture
    (destructuring-bind (pf sf url path)
	(publish-draft (draft-path "first") :generate-site nil)
      (is (stringp pf))
      (is (stringp sf))
      (is (stringp url))
      (is (stringp path))
      (let ((sfe (format nil "~Apost/2009/my_first_blog_post.xhtml" cl-blog-generator::*site-path*))
	    (pfe (format nil "~Apost/2009/my_first_blog_post.post" cl-blog-generator::*published-path*)))
	(is (string= sfe (namestring sf)))
	(is (string= pfe (namestring pf)))
	(is (cl-fad:file-exists-p sfe))
	(is (cl-fad:file-exists-p pfe))))))