(in-package #:cl-blog-generator-test)

(declaim (optimize (debug 3) (safety 2) (speed 1)))
(in-suite test)

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

(deftest test-%published-file-for-draft-file ()
  (let* ((in-path "/users/home/duncan/blog/draft/apost.post")
	 (expected-path "/users/home/duncan/blog/draft/../published/apost.post"))
    (is (string= expected-path (namestring (cl-blog-generator::%published-file-for-draft-file in-path))))))
