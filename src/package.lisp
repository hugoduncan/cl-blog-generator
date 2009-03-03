(defpackage #:cl-blog-generator
  (:use #:common-lisp)
  (:nicknames #:blog-gen)
  (:export #:publish-draft
	   #:set-environment
	   #:configure
	   #:*blog-db-spec*
	   #:*blog-domain*
	   #:*blog-root-path*
	   #:*site-path*
	   #:*published-path*
	   #:*template-path*))

