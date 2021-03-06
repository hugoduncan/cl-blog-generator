(defpackage #:my-blog
  (:use #:common-lisp #:cl-blog-generator))

(in-package #:my-blog)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *development-environment*
    '((:blog-title . "cl-blog-generator Development Blog")
      (:blog-db-spec . (:postmodern (:postgresql "127.0.0.1" "blog_dev" "duncan" "")))
      (:blog-domain . "hugoduncan.github.com")
      (:blog-root-path . "/cl-blog-generator/content/site/") ;; should end in /
      (:site-path . #p"/Users/duncan/projects/blog/content/site/")
      (:published-path . #p"/Users/duncan/projects/blog/content/published/")
      (:template-path . #p"/Users/duncan/projects/blog/content/template/"))
    "My development environment")

  (defparameter *test-environment*
    '((:blog-title . "test blog")
      (:blog-db-spec . (:postmodern (:postgresql "127.0.0.1" "blog_test" "duncan" "")))
      (:blog-domain . "hugoduncan.org")
      (:blog-root-path . "/blog/") ;; should end in /
      (:site-path . #p"/Users/duncan/projects/blog/test/site/")
      (:published-path . #p"/Users/duncan/projects/blog/test/published/")
      (:template-path . #p"/Users/duncan/projects/blog/test/template/"))
    "My test environment")

  (set-environment :test *test-environment*)
  (set-environment :development *development-environment*)
  (configure :test))

