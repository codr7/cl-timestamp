(defpackage timestamp
  (:use cl)
  (:export day days-in-month
	   get-month
	   hours
	   leap-year?
	   microseconds minutes month month-index
	   new-timestamp 
	   seconds
	   timestamp timestamp= timestamp< timestamp>
	   year
	   test))

(in-package timestamp)

(defparameter +months+ '#(:n/a :jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec))

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

(defun leap-year? (year)
  (and (zerop (mod year 4)) (not (zerop (mod year 100)))))

(defun days-in-month (month year)
  (ecase month
    (:jan 31)
    (:feb (if (leap-year? year) 29 28))
    (:mar 31)
    (:apr 30)
    (:may 31)
    (:jun 30)
    (:jul 31)
    (:aug 31)
    (:sep 30)
    (:oct 31)
    (:nov 30)
    (:dec 31)))
  
(defun get-month (i)
  (aref +months+ i))

(defun month-index (month)
  (position month +months+))
  
(defun timestamp= (x y)
  (and (= (year x) (year y))
       (eq (month x) (month y))
       (= (day x) (day y))
       (= (hours x) (hours y))
       (= (minutes x) (minutes y))
       (= (seconds x) (seconds y))
       (= (microseconds x) (microseconds y))))

(defun timestamp< (x y)
  (or (< (year x) (year y))
      (< (month-index (month x)) (month-index (month y)))
      (< (day x) (day y))
      (< (hours x) (hours y))
      (< (minutes x) (minutes y))
      (< (seconds x) (seconds y))
      (< (microseconds x) (microseconds y))))

(defun timestamp> (x y)
  (or (> (year x) (year y))
      (> (month-index (month x)) (month-index (month y)))
      (> (day x) (day y))
      (> (hours x) (hours y))
      (> (minutes x) (minutes y))
      (> (seconds x) (seconds y))
      (> (microseconds x) (microseconds y))))

(defun test ()
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
