(defpackage timestamp-test
  (:use cl timestamp)
  (:export run))

(in-package timestamp-test)

(defun run ()
  (let ((x (new-timestamp)))
    (assert (timestamp= x x)))

  (let ((x (new-timestamp 2000 :jan 1))
	(y (new-timestamp 2000 :jan 2)))
    (assert (timestamp< x y))
    (assert (not (timestamp< y x)))
    (assert (timestamp> y x))
    (assert (not (timestamp> x y))))

  (assert (= (days-in-month :feb 2000) 28))
  (assert (= (days-in-month :feb 2016) 29)))
