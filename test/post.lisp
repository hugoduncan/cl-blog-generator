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


(defvar *fake-names*
  '("Aaliyah" "Aaron" "Abagail" "Abbey" "Abbie" "Abbigail" "Abby" "Abdiel" "Abdul" "Abdullah" "Abe" "Abel" "Abelardo" "Abigail" "Abigale" "Abigayle" "Abner" "Abraham" "Ada" "Adah" "Adalberto" "Adaline" "Adam" "Adan" "Addie" "Addison" "Adela" "Adelbert" "Adele" "Adelia" "Adeline" "Adell" "Adella" "Adelle" "Aditya" "Adolf" "Adolfo" "Adolph" "Adolphus" "Adonis" "Adrain" "Adrian" "Adriana" "Adrianna" "Adriel" "Adrien" "Adrienne" "Afton" "Aglae" "Agnes" "Agustin" "Agustina" "Ahmad" "Ahmed" "Aida" "Aidan" "Aiden" "Aileen" "Aimee" "Aisha" "Aiyana" "Akeem" "Al" "Alaina" "Alan" "Alana" "Alanis" "Alanna" "Alayna" "Alba" "Albert" "Alberta" "Albertha" "Alberto" "Albin" "Albina" "Alda" "Alden" "Alec" "Aleen" "Alejandra" "Alejandrin" "Alek" "Alena" "Alene" "Alessandra" "Alessandro" "Alessia" "Aletha" "Alex" "Alexa" "Alexander" "Alexandra" "Alexandre" "Alexandrea" "Alexandria" "Alexandrine" "Alexandro" "Alexane" "Alexanne" "Alexie" "Alexis" "Alexys" "Alexzander" "Alf" "Alfonso" "Alfonzo" "Alford" "Alfred" "Alfreda" "Alfredo" "Ali" "Alia" "Alice" "Alicia" "Alisa" "Alisha" "Alison" "Alivia" "Aliya" "Aliyah" "Aliza" "Alize" "Allan" "Allen" "Allene" "Allie"))

(defun random-element (list)
  (nth (random (length list)) list))

(defun fake-name ()
  (random-element *fake-names*))

(defmacro with-test-db (&body body)
`(progn
   (configure :test)
   (elephant:with-open-store (*blog-db-spec*)
     (elephant:with-transaction ()
       ,@body))))

;;;# Fixtures to revert output state
(defun drop-all ()
  (with-test-db
    (loop for class in '(cl-blog-generator::blog-post cl-blog-generator::page
			 cl-blog-generator::index-page cl-blog-generator::tag-page
			 cl-blog-generator::generated-content cl-blog-generator::comment)
       do (elephant:drop-instances
	   (elephant:get-instances-by-class class)))
    ( )))

(defun draft-path (filename &key (type "post"))
  (merge-pathnames
   (make-pathname :directory '(:relative ".." "drafts") :name filename :type type)
   *site-path*))

(deftest test-draft-path ()
  (let ((*site-path* #p"/a/site/"))
    (is (string= (namestring (draft-path "fred")) "/a/site/../drafts/fred.post"))))

(defixture delete-all-fixture
    (:setup
     (drop-all)
     (loop for dir in (list *site-path* *published-path*)
	do
	  (ensure-directories-exist dir)
	  (cl-fad:walk-directory dir #'delete-file))))

(defixture test-environment-fixture
    (:setup
     (configure :test)))

(defun synopsis= (a b)
  (is (string= a (babel:octets-to-string b :encoding :utf-8))))

(defparameter *template-tests*
  '("fred<span>blogs</span>xxx"))


(deftest test-split-fmt ()
  (let ((components (cl-blog-generator::split-fmt nil)))
    (is (null components)))
  (let ((components (cl-blog-generator::split-fmt "one component")))
    (is (listp components))
    (is (= 1 (length components)))
    (is (string= "one component" (first components))))
  (let ((components (cl-blog-generator::split-fmt "first component|second component")))
    (is (listp components))
    (is (= 2 (length components)))
    (is (string= "first component" (first components)))
    (is (string= "second component" (second components))))
  (let ((components (cl-blog-generator::split-fmt "first component|second component|third component")))
    (is (listp components))
    (is (= 3 (length components)))
    (is (string= "first component" (first components)))
    (is (string= "second component" (second components)))
    (is (string= "third component" (third components))))
  (let ((components (cl-blog-generator::split-fmt "first|second|")))
    (is (listp components))
    (is (= 3 (length components)))
    (is (string= "first" (first components)))
    (is (string= "second" (second components)))
    (is (string= "" (third components)))))


(deftest test-merge-assoc ()
  (is (equalp '((:a . :a) (:b . :bb) (:c . :cc))
	      (sort (cl-blog-generator::merge-assoc '((:a . :a) (:b . :b))
						    '((:b . :bb) (:c . :cc)))
		    #'(lambda (x y)
			(string< (symbol-name (car x))
				 (symbol-name (car y)))))))
  (is (equalp '(("a" . "a") ("b" . "bb") ("c" . "cc"))
	      (sort (cl-blog-generator::merge-assoc '(("a" . "a") ("b" . "b"))
						    '(("b" . "bb") ("c" . "cc"))
						    :test #'string=)
		    #'(lambda (x y)
			(string< (car x) (car y)))))))

(deftest test-output-content-using-template-case (content)
  (let ((template (babel:string-to-octets (format nil "<div>~A</div>" content)))
	(output (cxml:make-octet-vector-sink)))
    (cl-blog-generator::output-content-using-template nil template output)
    (let ((result (sax:end-document output)))
      (is (string= content (babel:octets-to-string result :encoding :utf-8))))))

(deftest test-output-content-using-template ()
  (loop for i in *template-tests* do (test-output-content-using-template-case i)))


(deftest test-%parse-post-info-first ()
  (with-fixture test-environment-fixture
    (multiple-value-bind (title when updated tags linkname description synopsis template)
	(cl-blog-generator::%parse-post-info (draft-path "first"))
      (is (string= title "My First Blog Post"))
      (is (equalp '(24 02 2009) when))
      (is (equalp '("lisp" "blog") tags))
      (is (not linkname))
      (is (not updated))
      (is (not description))
      (is (synopsis= "<p>My first post.  Mainly to have something to use in developing the code.</p>" synopsis))
      (is (not template)))))

(deftest test-%parse-post-info-second ()
  (with-fixture test-environment-fixture
    (multiple-value-bind (title when updated tags linkname description synopsis template)
	(cl-blog-generator::%parse-post-info (draft-path "second"))
      (is (string= title "My Second Blog Post"))
      (is (equalp '(26 02 2009) when))
      (is (equalp '("lisp" "blog") tags))
      (is (string= linkname "a_second_blog_post_with_an_explicit_linkname"))
      (is (equalp '(27 02 2009) updated))
      (is (string= description  "A description"))
      (is (synopsis= "<p>My second post.  With an explicit linkname, and an updated tag.</p>" synopsis))
      (is (not template)))))


(deftest test-%parse-post-info-first-page ()
  (with-fixture test-environment-fixture
    (multiple-value-bind (title when updated tags linkname description synopsis template)
	(cl-blog-generator::%parse-post-info (draft-path "first" :type "page"))
      (is (string= title "My First Template Page"))
      (is (null when))
      (is (equalp '("lisp") tags))
      (is (string= "first_page" linkname))
      (is (not updated))
      (is (not description))
      (is (synopsis= "<p>My first non blog page.</p>" synopsis))
      (is (string= template "page")))))


(deftest test-%publish-draft-first ()
  (with-fixture delete-all-fixture
    (with-test-db
      (multiple-value-bind (output-path blog-post)
	  (cl-blog-generator::%publish-draft (draft-path "first"))
	(is (cl-fad:file-exists-p output-path))
	(is (string= "My First Blog Post"
		     (cl-blog-generator::content-title blog-post)))
	(is (string= "my_first_blog_post"
		     (cl-blog-generator::content-filename blog-post)))
	(is (equalp '(24 02 2009)
		    (cl-blog-generator::decode-date
		     (cl-blog-generator::content-when blog-post))))
	(is (null (cl-blog-generator::content-updated blog-post)))
	(is (string= "http://hugoduncan.org/blog/post/2009/my_first_blog_post.xhtml"
		     (funcall cl-blog-generator::*id-generator-fn* blog-post)))

	(multiple-value-bind (title when updated tags linkname description synopsis)
	    (cl-blog-generator::%parse-post-info output-path)
	  (is (string= title "My First Blog Post"))
	  (is (equalp '(24 02 2009) when))
	  (is (equalp '("lisp" "blog") tags))
	  (is (string= linkname "my_first_blog_post"))
	  (is (null updated))
	  (is (string= description "My first post.  Mainly to have something to use in developing the code."))
	  (is (synopsis= "<p>My first post.  Mainly to have something to use in developing the code.</p>" synopsis))
	  )))))




(deftest test-%publish-draft-second ()
  (with-fixture delete-all-fixture
    (with-test-db
      (multiple-value-bind (output-path blog-post)
	  (cl-blog-generator::%publish-draft (draft-path "second"))
	(is (cl-fad:file-exists-p output-path))
	(is (string= "My Second Blog Post"
		     (cl-blog-generator::content-title blog-post)))
	(is (string= "a_second_blog_post_with_an_explicit_linkname"
		     (cl-blog-generator::content-filename blog-post)))
	(is (equalp '(26 02 2009)
		    (cl-blog-generator::decode-date
		     (cl-blog-generator::content-when blog-post))))
	(is (equalp '(27 02 2009)
		    (cl-blog-generator::decode-date
		     (cl-blog-generator::content-updated blog-post))))
	(is (string= "http://hugoduncan.org/blog/post/2009/a_second_blog_post_with_an_explicit_linkname.xhtml"
		     (funcall cl-blog-generator::*id-generator-fn* blog-post)))))))


(deftest test-%publish-draft-first-page ()
  (let ((post-updated ;; this could cause problems testing at midnight
	  (cl-blog-generator::decode-local-date (get-universal-time))))
    (with-fixture delete-all-fixture
      (with-test-db
	(multiple-value-bind (output-path content)
	    (cl-blog-generator::%publish-draft (draft-path "first" :type "page"))
	  (is (cl-fad:file-exists-p output-path))
	  (is (string= "My First Template Page"
		       (cl-blog-generator::content-title content)))
	  (is (string= "first_page"
		       (cl-blog-generator::content-filename content)))
	  (is (equalp post-updated
		      (cl-blog-generator::decode-date
		       (cl-blog-generator::content-when content))))
	  (is (null (cl-blog-generator::content-updated content)))
	  (is (string= "http://hugoduncan.org/blog/page/first_page.xhtml"
		       (funcall cl-blog-generator::*id-generator-fn* content))))))))



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
			 (cl-blog-generator::content-title blog-post)))
	    (is (string= "my_first_blog_post"
			 (cl-blog-generator::content-filename blog-post)))
	    (is (equalp '(24 02 2009)
			(cl-blog-generator::decode-date
			 (cl-blog-generator::content-when blog-post))))
	    (is (equalp post-updated
			(cl-blog-generator::decode-date
			 (cl-blog-generator::content-updated blog-post))))
	    (is (string= "http://hugoduncan.org/blog/post/2009/my_first_blog_post.xhtml"
			 (funcall cl-blog-generator::*id-generator-fn* blog-post)))

	    (multiple-value-bind (title when updated tags linkname description synopsis)
		(cl-blog-generator::%parse-post-info output-path)
	      (is (string= title "My First Blog Post"))
	      (is (equalp '(24 02 2009) when))
	      (is (equalp '("lisp" "blog") tags))
	      (is (string= linkname "my_first_blog_post"))
	      (is (equalp post-updated updated))
	      (is (string= description "My first post.  Mainly to have something to use in developing the code."))
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
		       (cl-blog-generator::content-title blog-post)))
	  (is (string= "a_second_blog_post_with_an_explicit_linkname"
		       (cl-blog-generator::content-filename blog-post)))
	  (is (equalp '(26 02 2009)
		      (cl-blog-generator::decode-date
		       (cl-blog-generator::content-when blog-post))))
	  (is (equalp '(27 02 2009)
		      (cl-blog-generator::decode-date
		       (cl-blog-generator::content-updated blog-post))))
	  (is (string= "http://hugoduncan.org/blog/post/2009/a_second_blog_post_with_an_explicit_linkname.xhtml"
		       (funcall cl-blog-generator::*id-generator-fn* blog-post)))


	  (multiple-value-bind (title when updated tags linkname description synopsis)
	      (cl-blog-generator::%parse-post-info output-path)
	    (is (string= "My Second Blog Post" title))
	    (is (equalp '(26 02 2009) when))
	    (is (equalp '("lisp" "blog") tags))
	    (is (string= "a_second_blog_post_with_an_explicit_linkname" linkname))
	    (is (equalp '(27 02 2009) updated))
	    (is (string= "A description" description))
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

;; (deftest test-%adjacent-posts-same-date ()
;;   (with-fixture delete-all-fixture
;;     (with-test-db
;;       (multiple-value-bind (output-path blog-post1)
;; 	  (cl-blog-generator::%publish-draft (draft-path "third"))
;; 	(declare (ignore output-path))
;; 	(multiple-value-bind (output-path blog-post2)
;; 	    (cl-blog-generator::%publish-draft (draft-path "fith"))
;; 	  (declare (ignore output-path))
;; 	  (multiple-value-bind (output-path blog-post3)
;; 	      (cl-blog-generator::%publish-draft (draft-path "sixth"))
;; 	    (declare (ignore output-path))
;; 	    (format *debug-io* "~A ~A ~A~%" blog-post1 blog-post2 blog-post3)
;; 	    (multiple-value-bind (prior next)
;; 		(cl-blog-generator::%adjacent-posts blog-post1)
;; 	      (is (null prior))
;; 	      (is (eql next blog-post3)))
;; 	    (multiple-value-bind (prior next)
;; 		(cl-blog-generator::%adjacent-posts blog-post2)
;; 	      (is (eql blog-post3 prior))
;; 	      (is (null next)))
;; 	    (multiple-value-bind (prior next)
;; 		(cl-blog-generator::%adjacent-posts blog-post3)
;; 	      (is (eql blog-post1 prior))
;; 	      (is (eql blog-post2 next )))
;; 	    ))))))

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
    (cl-blog-generator::publish-draft (draft-path "fith"))
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

(deftest test-publish-draft-first-page ()
  (with-fixture delete-all-fixture
    (destructuring-bind (pf sf url path)
	(publish-draft (draft-path "first" :type "page") :generate-site nil)
      (is (stringp pf))
      (is (stringp sf))
      (is (stringp url))
      (is (stringp path))
      (let ((sfe (format nil "~Apage/first_page.xhtml" cl-blog-generator::*site-path*))
	    (pfe (format nil "~Apage/first_page.page" cl-blog-generator::*published-path*)))
	(is (string= sfe (namestring sf)))
	(is (string= pfe (namestring pf)))
	(is (cl-fad:file-exists-p sfe))
	(is (cl-fad:file-exists-p pfe))))))

(deftest test-tag-page-protocol ()
  (with-fixture delete-all-fixture
    (with-test-db
      (let ((tag-page (make-instance 'cl-blog-generator::tag-page :tag "atag")))
	(is (string= (format nil "~Atag/atag.xhtml" cl-blog-generator::*site-path*)
		     (namestring (cl-blog-generator::site-file-path-for tag-page))))
	(is (string= "/blog/tag/atag.xhtml"
		     (namestring (cl-blog-generator::path-for tag-page))))))))

(deftest test-%ensure-tag-pages-for ()
  (with-fixture delete-all-fixture
    (with-test-db
      (multiple-value-bind (output-path blog-post)
	  (cl-blog-generator::%publish-draft (draft-path "first"))
	(declare (ignore output-path))
	(is (= 2 (length (cl-blog-generator::content-tags blog-post))))
	(is (= 2 (length (elephant:get-instances-by-class 'cl-blog-generator::tag-page))))
	(loop
	   for tag in (cl-blog-generator::content-tags blog-post)
	   for tag-page = (elephant:get-instance-by-value 'cl-blog-generator::tag-page
							  'cl-blog-generator::tag tag)
	   do
	     (is tag-page)
	     (is (= 1 (length (cl-blog-generator::tag-page-related-tags tag-page))))
	     (is (= 1 (length (cl-blog-generator::%tag-posts tag))))
	     (is (equal (first (cl-blog-generator::%tag-posts tag)) blog-post)))))))


(defvar *comment-texts*
  '("This is a comment"
    "This is a multi-paragraph comment

Another paragraph."
        "This is a multi-paragraph comment with http://somedomain.com/links .

Another paragraph http://somedomain.com/links."))

(defvar *comment-regexes*
  '("<p>This is a comment</p>"
    "<p>This is a multi-paragraph comment</p><p>Another paragraph.</p>"
    "<a href=\"http://somedomain.com/links\">http://somedomain.com/links</a>"))


(deftest test-add-comment ()
  (with-fixture delete-all-fixture
    (with-test-db
      (multiple-value-bind (output-path blog-post)
	  (cl-blog-generator::%publish-draft (draft-path "first"))
	(declare (ignore output-path))
	(loop for comment-text in *comment-texts*
	     for comment-regex in *comment-regexes*
	   do
	     (let* ((name (fake-name))
		    (comment (cl-blog-generator::add-comment
			      (cl-blog-generator::content-filename blog-post)
			      "123.123.123.123"
			      name
			      "ab.cd@ef.com"
			      "http://freds domain"
			      (get-universal-time)
			      comment-text)))
	       (is (eql blog-post (cl-blog-generator::content-page comment)))
;; 	       (format *debug-io* "~A~%" (namestring (cl-blog-generator::published-file-path-for comment)))
	       (is (string= name (cl-blog-generator::comment-name comment)))
	       (is (string= (format nil "~A~{~A/~}~A/~A_~A.comment"
				    *published-path*
				    *comment-path*
				    (cl-blog-generator::content-filename blog-post)
				    (cl-blog-generator::comment-when comment)
				    (cl-blog-generator::%sanitise-title name))
			    (namestring (cl-blog-generator::published-file-path-for comment))))
	       (is (probe-file (cl-blog-generator::published-file-path-for comment)))
	       (with-open-file (stream (cl-blog-generator::published-file-path-for comment))
		 (let ((line (read-line stream)))
		   (is (cl-ppcre:scan comment-regex line))))))
	(is (= 3 (cl-blog-generator::%btree-length (cl-blog-generator::content-comments blog-post))))
	(cl-blog-generator::generate blog-post)))))


