(defpackage #:cl-blog-generator
  (:use #:common-lisp)
  (:nicknames #:blogen)
  (:export #:publish-draft
	   #:generate-site
	   #:set-environment
	   #:configure
	   #:*blog-db-spec*
	   #:*blog-domain*
	   #:*blog-root-path*
	   #:*site-path*
	   #:*published-path*
	   #:*template-path*
	   #:*relative-path-fn*))

