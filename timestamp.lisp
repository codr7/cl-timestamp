(defpackage timestamp
  (:use cl)
  (:export day
	   get-month
	   hours
	   microseconds minutes month
	   new-timestamp 
	   seconds
	   timestamp timestamp=
	   year
	   test))

(in-package timestamp)

(defparameter months '#(:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec))

(defstruct (timestamp (:conc-name nil))
  (year (error "Missing year") :type integer)
  (month (error "Missing month") :type keyword)
  (day (error "Missing day") :type integer)
  (hours (error "Missing hours") :type integer)
  (minutes (error "Missing minutes") :type integer)
  (seconds (error "Missing seconds") :type integer)
  (microseconds (error "Missing microseconds") :type integer))

(defun new-timestamp (&optional (year 1) (month :jan) (day 1)
			(hours 0) (minutes 0) (seconds 0) (microseconds 0))
  (make-timestamp :year year :month month :day day
		  :hours hours :minutes minutes :seconds seconds :microseconds microseconds))

(defmethod print-object ((val timestamp) out)
  (format out "~a-~a-~a ~a:~a:~a.~a"
	  (year val) (month val) (day val)
	  (hours val) (minutes val) (seconds val) (microseconds val)))

(defun get-month (i)
  (aref months (1- i)))

(defun timestamp= (x y)
  (and (= (year x) (year y))
       (eq (month x) (month y))
       (= (day x) (day y))
       (= (hours x) (hours y))
       (= (minutes x) (minutes y))
       (= (seconds x) (seconds y))
       (= (microseconds x) (microseconds y))))

(defun test ()
  (let ((ts (new-timestamp)))
    (assert (timestamp= ts ts))))
