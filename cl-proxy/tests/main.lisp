(defpackage cl-proxy/tests/main
  (:use :cl
        :cl-proxy
        :rove))
(in-package :cl-proxy/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-proxy)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
