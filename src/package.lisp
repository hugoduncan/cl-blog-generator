(defpackage #:cl-blog-generator
  (:use #:common-lisp)
  (:nicknames #:blogen)
  (:export #:publish-draft
	   #:publish-updated-post
	   #:generate-site
	   #:set-environment
	   #:configure
	   #:moderate
	   #:*blog-db-spec*
	   #:*blog-domain*
	   #:*blog-root-path*
	   #:*site-path*
	   #:*published-path*
	   #:*template-path*
	   #:*relative-path-fn*
	   #:*mailbox-type*
	   #:*mailbox-args*))

